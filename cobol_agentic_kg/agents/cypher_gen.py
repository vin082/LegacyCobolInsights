"""
Cypher Generator Agent - Translates natural language to Cypher queries
Supports hybrid semantic + graph search for better query understanding
"""
import re
from typing import List, Dict, Any
from utils.state import CobolProcessingState
from utils.logger import logger
from utils.neo4j_client import neo4j_client
from utils.llm_factory import get_llm
from config.settings import settings


class CypherGeneratorAgent:
    """Agent responsible for generating Cypher queries"""

    def __init__(self):
        self._llm = None
        self._schema = None
        self._llm_provider = None
        self._llm_model = None
        self._embeddings = None  # Lazy-loaded for semantic search
        self._vector_index_available = None  # Cache for vector index check

    @property
    def llm(self):
        """Lazy initialization of LLM with provider change detection"""
        current_provider = settings.llm_provider
        current_model = settings.get_llm_model()

        # Reinitialize LLM if provider or model changed
        if (self._llm is None or
            self._llm_provider != current_provider or
            self._llm_model != current_model):

            logger.info(f"🔄 Cypher Gen: Initializing {current_provider}/{current_model}")
            self._llm = get_llm(temperature=0)
            self._llm_provider = current_provider
            self._llm_model = current_model

        return self._llm

    @property
    def schema(self):
        """Lazy initialization of schema"""
        if self._schema is None:
            self._schema = self._get_schema()
        return self._schema

    def process(self, state: CobolProcessingState) -> CobolProcessingState:
        """
        Generate Cypher query from natural language

        Args:
            state: Current processing state

        Returns:
            Updated state with generated Cypher query
        """
        user_query = state.get('user_query')
        if not user_query:
            return state

        logger.info(f"📝 CYPHER GENERATOR AGENT: Generating query for '{user_query}'")

        try:
            cypher = self._generate_cypher(user_query)

            return {
                **state,
                "generated_cypher": cypher,
                "stage": "cypher_generation",
                "status": "completed"
            }

        except Exception as e:
            logger.error(f"Cypher generation failed: {e}")
            return {
                **state,
                "generated_cypher": "",
                "stage": "cypher_generation",
                "status": "failed",
                "errors": state.get('errors', []) + [f"Cypher generation error: {str(e)}"]
            }

    def _generate_cypher(self, user_query: str) -> str:
        """
        Generate Cypher query using LLM with optional semantic enhancement

        This method automatically detects if semantic search would be beneficial
        and uses vector similarity to find relevant entities before generating Cypher.
        """

        # Check if this is a semantic query
        if self._is_semantic_query(user_query):
            logger.info("🧠 Detected semantic query, using hybrid approach")

            # Perform semantic search
            semantic_results = self._semantic_search(user_query, k=5)

            if semantic_results:
                # Use semantic results to enhance Cypher generation
                return self._enhance_cypher_with_semantic_hints(user_query, semantic_results)
            else:
                logger.info("No semantic results found, falling back to standard Cypher generation")

        # Standard Cypher generation (no semantic enhancement)
        prompt = f"""You are a Neo4j Cypher expert analyzing a mainframe legacy system knowledge graph.

Schema:
{self.schema}

CRITICAL RULES:
1. Node labels are case-sensitive:
   - Use "CobolProgram" (NOT "Cobolprogram")
   - Use "DataFile" (NOT "Datafile")
   - Use "Job", "Dataset", "Copybook", "Transaction", "Mapset", "ScreenMap"
   - Use "Procedure", "DataStructure", "Field"

2. Understanding the data model:
   - **Jobs** (JCL batch jobs) EXECUTE programs and ALLOCATE datasets
   - **Datasets** (physical files) are allocated via JCL DD statements
   - **DataFiles** (logical files) are COBOL FD definitions in programs
   - **Copybooks** define shared data structures, included by programs
   - **Transactions** (CICS) invoke programs for online processing
   - When user asks about "jobs" or "batch", use Job nodes
   - When user asks about "data files" in job context, use Dataset nodes
   - When user asks about "files" in COBOL context, use DataFile nodes

3. Use toLower() for string equality comparisons, but NOT with CONTAINS:
   - Equality: WHERE toLower(p.name) = toLower('CUSTMAST')
   - Contains: WHERE toLower(p.name) CONTAINS 'tran' (use lowercase literal)
   - NEVER: WHERE toLower(p.name) CONTAINS toLower('TRAN') - WRONG!

4. CRITICAL: Semantic Query Patterns
   - When user mentions MULTIPLE concepts, use AND logic to combine them:
     ❌ WRONG: WHERE p.summary CONTAINS 'transaction'  (ignores "credit card")
     ✅ RIGHT: WHERE (p.summary CONTAINS 'credit' OR p.summary CONTAINS 'card') 
                AND (p.summary CONTAINS 'transaction' OR p.name CONTAINS 'trn')
   - Match ALL keywords user mentioned, not just one
   - If user says "credit card transactions", search for BOTH concepts
   - If user says "payment processing batch", search for PAYMENT + PROCESS + BATCH
   - Use OR within each concept, AND between concepts

5. ALWAYS assign relationship to a variable when using type(r):
   - CORRECT: MATCH (a)-[r:REL]->(b) ... type(r)
   - WRONG: MATCH (a)-[:REL]->(b) ... type(r)

6. CRITICAL - Cypher Aggregation (NOT SQL):
   - NO "GROUP BY" clause in Cypher - it does NOT exist!
   - Grouping is IMPLICIT based on non-aggregated fields in RETURN
   - CORRECT: MATCH (p:CobolProgram) RETURN p.domain, count(p) ORDER BY p.domain
   - WRONG: MATCH (p:CobolProgram) RETURN p.domain, count(p) GROUP BY p.domain
   - CORRECT: MATCH (p) RETURN p.type, sum(p.value), avg(p.score) ORDER BY p.type
   - Rule: Any field in RETURN that's not an aggregation (count, sum, avg, etc.) becomes a grouping key
   - Multiple grouping: RETURN p.domain, p.complexity, count(p) (groups by both domain AND complexity)

7. CRITICAL - Cypher ORDER BY (NOT SQL):
   - NO "NULLS FIRST" or "NULLS LAST" in Cypher - they do NOT exist!
   - Use coalesce() to handle nulls in sorting instead
   - CORRECT: ORDER BY coalesce(p.complexity_score, 0) DESC, p.loc DESC
   - WRONG: ORDER BY p.complexity_score DESC NULLS LAST, p.loc DESC
   - CORRECT: ORDER BY coalesce(p.domain, 'UNKNOWN') ASC
   - WRONG: ORDER BY p.domain ASC NULLS FIRST

8. Common query patterns:

=== COBOL PROGRAMS ===
Program info:
MATCH (p:CobolProgram)
WHERE toLower(p.name) = toLower('PROGRAMNAME')
WITH p
OPTIONAL MATCH (caller:CobolProgram)-[:CALLS]->(p)
WITH p, collect(DISTINCT caller.name) AS CalledBy
OPTIONAL MATCH (j:Job)-[:EXECUTES]->(p)
WITH p, CalledBy, collect(DISTINCT j.name) AS BatchJobs
OPTIONAL MATCH (t:Transaction)-[:INVOKES]->(p)
WITH p, CalledBy, BatchJobs, collect(DISTINCT t.transid) AS OnlineTransactions
OPTIONAL MATCH (p)-[:READS|WRITES]->(f:DataFile)
WITH p, CalledBy, BatchJobs, OnlineTransactions, collect(DISTINCT f.name) AS DataFiles
OPTIONAL MATCH (p)-[:INCLUDES]->(c:Copybook)
WITH p, CalledBy, BatchJobs, OnlineTransactions, DataFiles, collect(DISTINCT c.name) AS Copybooks
RETURN p.name AS Program, p.summary, p.domain, p.complexity, p.loc,
       CalledBy AS CalledByPrograms, BatchJobs AS ExecutingJobs, 
       OnlineTransactions AS InvokingTransactions, DataFiles AS AffectedDataFiles, 
       Copybooks AS IncludedCopybooks

Programs called:
MATCH (p:CobolProgram)-[:CALLS]->(called:CobolProgram)
WHERE toLower(p.name) = toLower('PROGRAMNAME')
RETURN called.name, called.summary

Impact analysis (who calls X):
MATCH (caller:CobolProgram)-[:CALLS]->(p:CobolProgram)
WHERE toLower(p.name) = toLower('PROGRAMNAME')
RETURN caller.name, caller.summary

COBOL file operations (IMPORTANT - always assign relationship to variable 'r'):
MATCH (p:CobolProgram)-[r:READS|WRITES]->(f:DataFile)
WHERE toLower(f.name) = toLower('FILENAME')
RETURN p.name AS Program, type(r) AS Operation, f.name AS File

=== BATCH JOBS (JCL) ===
Find jobs by name pattern:
MATCH (j:Job)
WHERE toLower(j.name) CONTAINS 'acct'
RETURN j.name, j.step_count, j.dataset_count
ORDER BY j.name

Job execution flow (programs executed):
MATCH (j:Job)-[e:EXECUTES]->(p:CobolProgram)
WHERE toLower(j.name) = toLower('JOBNAME')
RETURN j.name, e.step AS Step, p.name AS Program, e.parm AS Parameters
ORDER BY e.step

Job dataset allocations:
MATCH (j:Job)-[a:ALLOCATES]->(ds:Dataset)
WHERE toLower(j.name) = toLower('JOBNAME')
RETURN j.name, a.ddname AS DDName, ds.dsn AS Dataset, ds.type AS Type

Jobs processing specific dataset (CRITICAL - use Dataset nodes, not DataFile):
MATCH (j:Job)-[a:ALLOCATES]->(ds:Dataset)
WHERE ds.dsn CONTAINS 'ACCTDATA'
WITH j, collect({{ddname: a.ddname, dsn: ds.dsn}}) as Datasets
MATCH (j)-[e:EXECUTES]->(p:CobolProgram)
RETURN j.name AS Job, j.step_count AS Steps, Datasets, collect({{step: e.step, program: p.name}}) AS ExecutionSteps

Most complex jobs (by steps and datasets):
MATCH (j:Job)
RETURN j.name, j.step_count, j.dataset_count
ORDER BY j.step_count DESC, j.dataset_count DESC
LIMIT 10

=== CICS TRANSACTIONS ===
Find transaction by ID:
MATCH (t:Transaction)
WHERE toLower(t.transid) = toLower('CCUP')
RETURN t.transid, t.description, t.program

Transactions invoking program:
MATCH (t:Transaction)-[:INVOKES]->(p:CobolProgram)
WHERE toLower(p.name) = toLower('PROGRAMNAME')
RETURN t.transid, t.description, p.name

All transactions with pattern:
MATCH (t:Transaction)
WHERE toLower(t.description) CONTAINS 'credit card'
RETURN t.transid, t.description, t.program
ORDER BY t.transid

Complete transaction flow (trans -> program -> copybooks):
MATCH (t:Transaction)-[:INVOKES]->(p:CobolProgram)
WHERE toLower(t.transid) = toLower('CCUP')
OPTIONAL MATCH (p)-[:INCLUDES]->(c:Copybook)
OPTIONAL MATCH (p)-[r:READS|WRITES]->(f:DataFile)
RETURN t.transid, t.description, p.name, collect(DISTINCT c.name) AS Copybooks, collect(DISTINCT f.name) AS DataFiles

=== COPYBOOKS ===
Programs using copybook (impact analysis):
MATCH (c:Copybook)<-[:INCLUDES]-(p:CobolProgram)
WHERE toLower(c.name) = toLower('CVACT01Y')
RETURN c.name, collect(p.name) AS AffectedPrograms

Copybook data structures and fields:
MATCH (c:Copybook)-[:DEFINES]->(ds:DataStructure)-[:HAS_FIELD]->(f:Field)
WHERE toLower(c.name) = toLower('COPYBOOKNAME')
RETURN c.name, ds.name, collect(f.name) AS Fields

Most used copybooks (high coupling):
MATCH (c:Copybook)<-[:INCLUDES]-(p:CobolProgram)
WITH c, count(p) AS ProgramCount
WHERE ProgramCount > 2
RETURN c.name, ProgramCount
ORDER BY ProgramCount DESC

=== MAPSETS & SCREEN MAPS (CICS UI) ===
List all mapsets (discovery):
MATCH (ms:Mapset)
RETURN ms.name, ms.map_count, ms.description
ORDER BY ms.name

Find mapset by exact name:
MATCH (ms:Mapset)
WHERE toLower(ms.name) = toLower('MAPSETNAME')
RETURN ms.name, ms.map_count, ms.description

Find mapset by pattern (use CONTAINS for partial matches):
MATCH (ms:Mapset)
WHERE toLower(ms.name) CONTAINS 'pattern' OR toLower(ms.description) CONTAINS 'pattern'
RETURN ms.name, ms.map_count, ms.description
ORDER BY ms.name

All maps in a mapset:
MATCH (ms:Mapset)-[:CONTAINS_MAP]->(sm:ScreenMap)
WHERE toLower(ms.name) = toLower('MAPSETNAME')
RETURN ms.name AS Mapset, sm.name AS ScreenMap, sm.field_count AS FieldCount
ORDER BY sm.name

Screen map fields:
MATCH (sm:ScreenMap)
WHERE toLower(sm.name) = toLower('SCREENMAPNAME')
OPTIONAL MATCH (sm)-[:HAS_FIELD]->(f:Field)
WITH sm, collect({{name: f.name, level: f.level, picture: f.picture, usage: f.usage}}) AS Fields
RETURN sm.name AS ScreenMap, sm.field_count AS FieldCount, Fields
ORDER BY sm.name

Programs using mapset:
MATCH (ms:Mapset)<-[:USES_MAPSET]-(p:CobolProgram)
WHERE toLower(ms.name) = toLower('MAPSETNAME')
RETURN ms.name, collect(p.name) AS Programs

=== SEMANTIC SEARCH (Multi-Concept Queries) ===
Credit card transaction processing (multiple keywords with AND):
MATCH (p:CobolProgram)
WHERE (toLower(p.summary) CONTAINS 'credit' OR toLower(p.summary) CONTAINS 'card' OR toLower(p.name) CONTAINS 'cc')
  AND (toLower(p.summary) CONTAINS 'transaction' OR toLower(p.name) CONTAINS 'trn')
RETURN p.name, p.summary, p.domain
ORDER BY p.name

Payment processing batch jobs (concept combination):
MATCH (j:Job)-[:EXECUTES]->(p:CobolProgram)
WHERE (toLower(p.summary) CONTAINS 'payment' OR toLower(p.summary) CONTAINS 'pay')
  AND (toLower(j.name) CONTAINS 'pay' OR toLower(p.name) CONTAINS 'pay')
RETURN j.name AS Job, p.name AS Program, p.summary
ORDER BY j.name

Account reconciliation processes (concept combination):
MATCH (p:CobolProgram)
WHERE (toLower(p.summary) CONTAINS 'account' OR toLower(p.summary) CONTAINS 'acct' OR toLower(p.name) CONTAINS 'acct')
  AND (toLower(p.summary) CONTAINS 'reconcil' OR toLower(p.summary) CONTAINS 'match' OR toLower(p.summary) CONTAINS 'balance')
OPTIONAL MATCH (j:Job)-[:EXECUTES]->(p)
RETURN p.name, p.summary, collect(DISTINCT j.name) AS ExecutingJobs
ORDER BY p.name

Customer master data management (multiple related keywords):
MATCH (p:CobolProgram)
WHERE (toLower(p.summary) CONTAINS 'customer' OR toLower(p.name) CONTAINS 'cust')
  AND (toLower(p.summary) CONTAINS 'master' OR toLower(p.summary) CONTAINS 'maint' OR toLower(p.summary) CONTAINS 'manage')
OPTIONAL MATCH (f:DataFile)<-[:READS|WRITES]-(p)
RETURN p.name, p.summary, count(DISTINCT f) AS FilesAccessed
ORDER BY p.name

=== END-TO-END LINEAGE ===
Complete job-to-field lineage:
MATCH (j:Job)-[:EXECUTES]->(p:CobolProgram)-[:INCLUDES]->(c:Copybook)-[:DEFINES]->(ds:DataStructure)-[:HAS_FIELD]->(f:Field)
WHERE toLower(j.name) = toLower('JOBNAME')
RETURN j.name, p.name, c.name, ds.name, collect(f.name) AS Fields

Cross-system analysis (batch + online):
MATCH (p:CobolProgram)
WHERE EXISTS((p)<-[:EXECUTES]-(:Job)) AND EXISTS((p)<-[:INVOKES]-(:Transaction))
OPTIONAL MATCH (j:Job)-[:EXECUTES]->(p)
OPTIONAL MATCH (t:Transaction)-[:INVOKES]->(p)
RETURN p.name, collect(DISTINCT j.name) AS BatchJobs, collect(DISTINCT t.transid) AS Transactions

Copybook impact across jobs and transactions:
MATCH (c:Copybook)<-[:INCLUDES]-(p:CobolProgram)
WHERE toLower(c.name) = toLower('COPYBOOKNAME')
OPTIONAL MATCH (j:Job)-[:EXECUTES]->(p)
OPTIONAL MATCH (t:Transaction)-[:INVOKES]->(p)
RETURN c.name, collect(DISTINCT p.name) AS Programs, collect(DISTINCT j.name) AS Jobs, collect(DISTINCT t.transid) AS Transactions

=== BUSINESS INSIGHTS & RISK ANALYSIS ===
Critical programs (high complexity, high reuse):
MATCH (p:CobolProgram)
WHERE p.complexity >= 'HIGH' OR p.loc > 5000
WITH p
OPTIONAL MATCH (caller:CobolProgram)-[:CALLS]->(p)
OPTIONAL MATCH (j:Job)-[:EXECUTES]->(p)
OPTIONAL MATCH (t:Transaction)-[:INVOKES]->(p)
RETURN p.name, p.loc AS LinesOfCode, p.complexity, 
       count(DISTINCT caller) AS CalledBy, collect(DISTINCT j.name) AS BatchJobs, collect(DISTINCT t.transid) AS Transactions
ORDER BY p.loc DESC

High-impact changes (programs affecting many files):
MATCH (p:CobolProgram)
WITH p
OPTIONAL MATCH (p)-[r:READS|WRITES]->(f:DataFile)
OPTIONAL MATCH (p)-[:ALLOCATES]->(d:Dataset)
WITH p, count(DISTINCT f) AS FileCount, count(DISTINCT d) AS DatasetCount
WHERE FileCount > 3 OR DatasetCount > 5
OPTIONAL MATCH (caller:CobolProgram)-[:CALLS]->(p)
RETURN p.name, FileCount, DatasetCount, count(DISTINCT caller) AS CalledBy
ORDER BY FileCount DESC, DatasetCount DESC

Data lineage (end-to-end: Job → Dataset → Program → File):
MATCH (j:Job)-[:ALLOCATES]->(ds:Dataset)
WITH j, ds
MATCH (j)-[:EXECUTES]->(p:CobolProgram)
WITH j, ds, p
OPTIONAL MATCH (p)-[:READS|WRITES]->(f:DataFile)
RETURN j.name AS Job, ds.dsn AS InputDataset, p.name AS Program, collect(DISTINCT f.name) AS FilesProcessed
ORDER BY j.name

System dependencies (critical batch jobs):
MATCH (j:Job)
WITH j
OPTIONAL MATCH (j)-[:EXECUTES]->(p:CobolProgram)
OPTIONAL MATCH (j)-[:ALLOCATES]->(ds:Dataset)
WITH j, count(DISTINCT p) AS ProgramCount, count(DISTINCT ds) AS DatasetCount
WHERE ProgramCount > 3 OR DatasetCount > 10
RETURN j.name, j.step_count, ProgramCount AS Programs, DatasetCount AS Datasets
ORDER BY j.step_count DESC

Modernization candidates (simple programs good for cloud):
MATCH (p:CobolProgram)
RETURN p.name, p.loc, p.complexity
ORDER BY p.loc ASC
LIMIT 20

Modernization candidates (low complexity):
MATCH (p:CobolProgram)
WHERE toLower(p.complexity) IN ['low', 'simple', 'minimal']
WITH p
OPTIONAL MATCH (p)-[:CALLS]->(called:CobolProgram)
OPTIONAL MATCH (p)-[:READS|WRITES]->(f:DataFile)
RETURN p.name, p.loc, p.complexity, count(DISTINCT called) AS CallsOtherPrograms, count(DISTINCT f) AS FileCount
ORDER BY p.loc ASC

Modernization candidates (fewest dependencies):
MATCH (p:CobolProgram)
WITH p
OPTIONAL MATCH (p)-[:CALLS]->(called:CobolProgram)
OPTIONAL MATCH (p)-[:READS|WRITES]->(f:DataFile)
OPTIONAL MATCH (caller:CobolProgram)-[:CALLS]->(p)
WITH p, count(DISTINCT called) AS CallCount, count(DISTINCT f) AS FileCount, count(DISTINCT caller) AS CallerCount
WHERE CallCount <= 2 AND FileCount <= 3 AND CallerCount <= 2
RETURN p.name, p.loc, p.complexity, CallCount AS Calls, FileCount AS Files, CallerCount AS CalledBy
ORDER BY FileCount ASC, CallCount ASC
LIMIT 20

Copybook reuse (coupling analysis - high reuse = high risk):
MATCH (c:Copybook)<-[:INCLUDES]-(p:CobolProgram)
WITH c, count(p) AS ProgramCount
WHERE ProgramCount > 2
OPTIONAL MATCH (c)-[:DEFINES]->(ds:DataStructure)
WITH c, ProgramCount, count(DISTINCT ds) AS StructCount
RETURN c.name, ProgramCount AS UsedBy, StructCount AS Structures
ORDER BY ProgramCount DESC

Call chain depth (long chains = hard to understand):
MATCH path=(:CobolProgram)-[:CALLS*1..5]->(:CobolProgram)
WITH path, length(path) AS Depth
WHERE Depth > 2
WITH [node IN nodes(path) | node.name] AS CallChain, Depth
RETURN CallChain, Depth
ORDER BY Depth DESC
LIMIT 10

Call chain by depth level:
MATCH (p:CobolProgram)-[:CALLS*1..3]->(target:CobolProgram)
WITH p, target, length(apoc.path.expandConfig({{startNode: p, relationshipFilter: 'CALLS>', endNode: target}})) AS Depth
RETURN p.name AS StartProgram, target.name AS EndProgram, Depth
ORDER BY Depth DESC
LIMIT 20

Longest call chains (simplified):
MATCH (p1:CobolProgram)-[:CALLS]->(p2:CobolProgram)
OPTIONAL MATCH (p2)-[:CALLS]->(p3:CobolProgram)
OPTIONAL MATCH (p3)-[:CALLS]->(p4:CobolProgram)
OPTIONAL MATCH (p4)-[:CALLS]->(p5:CobolProgram)
WITH p1, p2, p3, p4, p5
WHERE p2 IS NOT NULL AND p3 IS NOT NULL AND p4 IS NOT NULL
RETURN p1.name, p2.name, p3.name, p4.name, p5.name AS CallChain
ORDER BY p1.name

Online vs batch coupling (programs doing both = risk):
MATCH (p:CobolProgram)
WHERE EXISTS((p)<-[:EXECUTES]-(:Job)) AND EXISTS((p)<-[:INVOKES]-(:Transaction))
WITH p
OPTIONAL MATCH (j:Job)-[:EXECUTES]->(p)
OPTIONAL MATCH (t:Transaction)-[:INVOKES]->(p)
RETURN p.name, collect(DISTINCT j.name) AS BatchJobs, collect(DISTINCT t.transid) AS OnlineTransactions
ORDER BY p.name

Dataset volatility (files written by multiple programs = potential conflicts):
MATCH (p:CobolProgram)-[:WRITES]->(f:DataFile)
WITH f, count(DISTINCT p) AS WriterCount
WHERE WriterCount > 1
OPTIONAL MATCH (reader:CobolProgram)-[:READS]->(f)
RETURN f.name, WriterCount AS WritingPrograms, count(DISTINCT reader) AS ReadingPrograms
ORDER BY WriterCount DESC

=== SYSTEM OVERVIEW ===
List all node types:
MATCH (n)
RETURN DISTINCT labels(n) AS NodeType, count(*) AS Count
ORDER BY Count DESC

Programs by business domain (CRITICAL - NO GROUP BY in Cypher):
MATCH (p:CobolProgram)
RETURN p.domain AS BusinessDomain, count(p) AS ProgramCount
ORDER BY ProgramCount DESC

Programs by complexity (implicit grouping):
MATCH (p:CobolProgram)
RETURN p.complexity AS ComplexityLevel, count(p) AS Count
ORDER BY ComplexityLevel

List all jobs:
MATCH (j:Job)
RETURN j.name, j.step_count, j.dataset_count
ORDER BY j.name

List all transactions:
MATCH (t:Transaction)
RETURN t.transid, t.description, t.program
ORDER BY t.transid

Semantic search (when user asks about business concepts):
MATCH (p:CobolProgram)
WHERE toLower(p.summary) CONTAINS 'transaction' OR toLower(p.name) CONTAINS 'trn'
RETURN p.name, p.summary, p.domain, p.loc
ORDER BY p.name

IMPORTANT SEMANTIC QUERY GUIDELINES:
- When user asks about "credit card transactions" or "processing transactions":
  Use: WHERE toLower(p.summary) CONTAINS 'transaction' OR toLower(p.name) CONTAINS 'trn'
- When user asks about "customers" or "customer management":
  Use: WHERE toLower(p.summary) CONTAINS 'customer' OR toLower(p.name) CONTAINS 'cust'
- When user asks about "accounts":
  Use: WHERE toLower(p.summary) CONTAINS 'account' OR toLower(p.name) CONTAINS 'acct'
- When user asks about "batch processing":
  Use: WHERE toLower(p.name) STARTS WITH 'cb'
- Use CONTAINS with keywords in summary field, NOT exact domain matching
- Combine multiple conditions with OR to cast a wider net

User Question: {user_query}

Return ONLY the Cypher query, no explanation or markdown:
"""

        response = self.llm.invoke(prompt)
        cypher = response.content.strip()

        # Clean markdown if present
        if cypher.startswith('```'):
            cypher = cypher.split('```')[1]
            if cypher.startswith('cypher'):
                cypher = cypher[6:]
            cypher = cypher.strip()

        # Normalize whitespace: replace multiple spaces/newlines with single space
        # This ensures the query works in Neo4j Browser when copied
        cypher = re.sub(r'\s+', ' ', cypher)
        cypher = cypher.strip()

        return cypher

    def _get_schema(self) -> str:
        """Get current graph schema"""
        try:
            return neo4j_client.graph.schema
        except:
            return "Schema not available"

    # ========================================================================
    # HYBRID SEMANTIC SEARCH METHODS
    # ========================================================================

    @property
    def embeddings(self):
        """Lazy initialization of embeddings for semantic search"""
        if self._embeddings is None:
            try:
                from langchain_openai import OpenAIEmbeddings
                self._embeddings = OpenAIEmbeddings(model="text-embedding-3-small")
                logger.debug("Initialized embeddings for semantic search")
            except Exception as e:
                logger.warning(f"Failed to initialize embeddings: {e}")
                self._embeddings = None
        return self._embeddings

    def _check_vector_index_available(self) -> bool:
        """Check if vector index exists for semantic search"""
        if self._vector_index_available is not None:
            return self._vector_index_available

        try:
            result = neo4j_client.query("SHOW INDEXES WHERE name = 'cobol_program_embeddings'")
            self._vector_index_available = len(result) > 0
            if self._vector_index_available:
                logger.debug("Vector index available for semantic search")
            else:
                logger.debug("Vector index not found - semantic search disabled")
            return self._vector_index_available
        except:
            self._vector_index_available = False
            return False

    def _is_semantic_query(self, query: str) -> bool:
        """
        Determine if query would benefit from semantic search

        Semantic queries are those that:
        - Use conceptual terms (risky, complex, similar)
        - Ask about business domains without exact names
        - Use fuzzy matching (like, related, handling)
        """
        semantic_keywords = [
            'like', 'similar', 'related', 'risky', 'complex',
            'handling', 'managing', 'processing', 'dealing with',
            'high risk', 'legacy', 'difficult', 'problematic',
            'comparable to', 'resembles', 'alike'
        ]

        query_lower = query.lower()
        return any(keyword in query_lower for keyword in semantic_keywords)

    def _semantic_search(self, query: str, k: int = 5) -> List[Dict[str, Any]]:
        """
        Perform semantic vector similarity search

        Args:
            query: Natural language query
            k: Number of results to return

        Returns:
            List of {name, summary, score} dictionaries
        """
        if not self.embeddings or not self._check_vector_index_available():
            logger.debug("Semantic search not available")
            return []

        try:
            # Generate embedding for query
            query_embedding = self.embeddings.embed_query(query)

            # Vector similarity search
            results = neo4j_client.query("""
            CALL db.index.vector.queryNodes('cobol_program_embeddings', $k, $embedding)
            YIELD node, score
            WHERE score > 0.7
            RETURN node.name as name,
                   node.summary as summary,
                   node.domain as domain,
                   node.complexity as complexity,
                   score
            ORDER BY score DESC
            """, {'k': k, 'embedding': query_embedding})

            logger.info(f"🔍 Semantic search found {len(results)} similar programs")
            for r in results[:3]:
                logger.debug(f"   - {r['name']} (score: {r['score']:.3f})")

            return results

        except Exception as e:
            logger.warning(f"Semantic search failed: {e}")
            return []

    def _enhance_cypher_with_semantic_hints(self, user_query: str, semantic_results: List[Dict[str, Any]]) -> str:
        """
        Generate Cypher using semantic search results as hints

        Instead of pure text-to-Cypher, we use semantic search to find relevant
        entities, then generate Cypher targeting those entities.
        """
        if not semantic_results:
            # Fallback to regular generation
            return self._generate_cypher(user_query)

        # Extract program names from semantic results
        program_names = [r['name'] for r in semantic_results[:5]]

        # Enhanced prompt with semantic hints
        prompt = f"""You are a Neo4j Cypher expert analyzing a mainframe legacy system knowledge graph.

Schema:
{self.schema}

User Question: {user_query}

SEMANTIC SEARCH HINTS:
The following programs were found to be semantically similar to the query:
{', '.join(program_names)}

Generate a Cypher query that:
1. Focuses on these programs as they match the user's intent
2. Uses these program names with IN operator for filtering
3. Returns relevant details about these programs and their relationships

Return ONLY the Cypher query, no explanation:
"""

        response = self.llm.invoke(prompt)
        cypher = response.content.strip()

        # Clean markdown if present
        if cypher.startswith('```'):
            cypher = cypher.split('```')[1]
            if cypher.startswith('cypher'):
                cypher = cypher[6:]
            cypher = cypher.strip()

        # Normalize whitespace
        cypher = re.sub(r'\s+', ' ', cypher)
        cypher = cypher.strip()

        logger.info(f"💡 Enhanced Cypher with semantic hints: {len(program_names)} programs")
        return cypher


# Create singleton instance
cypher_generator_agent = CypherGeneratorAgent()


# Wrapper function for LangGraph
def cypher_generator_agent_node(state: CobolProcessingState) -> CobolProcessingState:
    """LangGraph node wrapper"""
    return cypher_generator_agent.process(state)
