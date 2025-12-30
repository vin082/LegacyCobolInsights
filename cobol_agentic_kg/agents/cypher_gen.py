"""
Cypher Generator Agent - Translates natural language to Cypher queries
"""
from langchain_openai import ChatOpenAI
from utils.state import CobolProcessingState
from utils.logger import logger
from utils.neo4j_client import neo4j_client
from config.settings import settings


class CypherGeneratorAgent:
    """Agent responsible for generating Cypher queries"""

    def __init__(self):
        self._llm = None
        self._schema = None

    @property
    def llm(self):
        """Lazy initialization of LLM"""
        if self._llm is None:
            self._llm = ChatOpenAI(model="gpt-4o", temperature=0)
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
        """Generate Cypher query using LLM"""

        prompt = f"""You are a Neo4j Cypher expert analyzing a COBOL knowledge graph.

Schema:
{self.schema}

CRITICAL RULES:
1. Node labels are case-sensitive:
   - Use "CobolProgram" (NOT "Cobolprogram")
   - Use "DataFile" (NOT "Datafile")
   - Use "Procedure" and "Variable"

2. Use toLower() for string equality comparisons, but NOT with CONTAINS:
   - Equality: WHERE toLower(p.name) = toLower('CUSTMAST')
   - Contains: WHERE toLower(p.name) CONTAINS 'tran' (use lowercase literal)
   - NEVER: WHERE toLower(p.name) CONTAINS toLower('TRAN') - WRONG!

3. ALWAYS assign relationship to a variable when using type(r):
   - CORRECT: MATCH (a)-[r:REL]->(b) ... type(r)
   - WRONG: MATCH (a)-[:REL]->(b) ... type(r)

4. Common query patterns:

Program info:
MATCH (p:CobolProgram)
WHERE toLower(p.name) = toLower('PROGRAMNAME')
RETURN p.name, p.summary, p.domain, p.complexity, p.loc

Programs called:
MATCH (p:CobolProgram)-[:CALLS]->(called:CobolProgram)
WHERE toLower(p.name) = toLower('PROGRAMNAME')
RETURN called.name, called.summary

Impact analysis (who calls X):
MATCH (caller:CobolProgram)-[:CALLS]->(p:CobolProgram)
WHERE toLower(p.name) = toLower('PROGRAMNAME')
RETURN caller.name, caller.summary

File operations (IMPORTANT - always assign relationship to variable 'r'):
MATCH (p:CobolProgram)-[r:READS|WRITES]->(f:DataFile)
WHERE toLower(f.name) = toLower('FILENAME')
RETURN p.name AS Program, type(r) AS Operation, f.name AS File

Call chain:
MATCH path = (p:CobolProgram)-[:CALLS*1..3]->(end:CobolProgram)
WHERE toLower(p.name) = toLower('PROGRAMNAME')
RETURN [node in nodes(path) | node.name] as call_chain

High complexity programs:
MATCH (p:CobolProgram)
WHERE p.complexity_score > 50
RETURN p.name, p.complexity_score, p.domain
ORDER BY p.complexity_score DESC
LIMIT 10

Programs with name pattern (CONTAINS):
MATCH (p:CobolProgram)
WHERE toLower(p.name) CONTAINS 'tran'
RETURN p.name, p.summary, p.loc
ORDER BY p.name

List all programs:
MATCH (p:CobolProgram)
RETURN p.name, p.summary, p.loc
ORDER BY p.name

List all files:
MATCH (f:DataFile)
RETURN f.name
ORDER BY f.name

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

        return cypher

    def _get_schema(self) -> str:
        """Get current graph schema"""
        try:
            return neo4j_client.graph.schema
        except:
            return "Schema not available"


# Create singleton instance
cypher_generator_agent = CypherGeneratorAgent()


# Wrapper function for LangGraph
def cypher_generator_agent_node(state: CobolProcessingState) -> CobolProcessingState:
    """LangGraph node wrapper"""
    return cypher_generator_agent.process(state)
