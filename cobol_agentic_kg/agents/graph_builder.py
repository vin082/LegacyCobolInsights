"""
Graph Builder Agent - Constructs Neo4j knowledge graph

This agent creates nodes and relationships in Neo4j for different file types:
- COBOL programs: CobolProgram nodes with CALLS, READS, WRITES relationships
- Copybooks: Copybook nodes with data structures and INCLUDES relationships
- JCL: Job nodes with EXECUTES and ALLOCATES relationships
- CICS: Transaction and ScreenMap nodes with INVOKES relationships

The graph structure enables:
- Dependency analysis
- Impact analysis
- Data lineage tracking
- Transaction flow visualization
"""
from typing import Dict, Any, List
from utils.state import CobolProcessingState
from utils.neo4j_client import neo4j_client
from utils.logger import logger


class GraphBuilderAgent:
    """Agent responsible for building the knowledge graph"""

    def __init__(self):
        self.client = neo4j_client

    def process(self, state: CobolProcessingState) -> CobolProcessingState:
        """
        Build knowledge graph from parsed data

        Routes to appropriate builder based on which data is present:
        - parsed_data -> build_cobol_graph()
        - copybook_data -> build_copybook_graph()
        - jcl_data -> build_jcl_graph()
        - cics_data -> build_cics_graph()

        Args:
            state: Current processing state

        Returns:
            Updated state with graph statistics
        """
        file_name = state['file_metadata']['filename']
        logger.info(f"🕸️  GRAPH BUILDER AGENT: Creating graph for {file_name}")

        try:
            stats = {
                "nodes_created": 0,
                "relationships_created": 0
            }

            # Route to appropriate graph builder based on data type
            if state.get('parsed_data') and state['parsed_data']:
                # COBOL program
                self._build_cobol_graph(state, stats)

            elif state.get('copybook_data') and state['copybook_data']:
                # Copybook
                self._build_copybook_graph(state, stats)

            elif state.get('jcl_data') and state['jcl_data']:
                # JCL
                self._build_jcl_graph(state, stats)

            elif state.get('cics_data') and state['cics_data']:
                # CICS (BMS or CSD)
                self._build_cics_graph(state, stats)

            else:
                logger.warning(f"No parseable data found in state for {file_name}")

            logger.info(f"✅ Created {stats['nodes_created']} nodes, {stats['relationships_created']} relationships")

            # Update state
            return {
                **state,
                "graph_data": stats,
                "stage": "graph_building",
                "status": "completed"
            }

        except Exception as e:
            logger.error(f"❌ Graph building failed: {e}")
            return {
                **state,
                "graph_data": {"nodes_created": 0, "relationships_created": 0},
                "stage": "graph_building",
                "status": "failed",
                "errors": state.get('errors', []) + [f"Graph building error: {str(e)}"]
            }

    # ========================================================================
    # COBOL PROGRAM GRAPH BUILDER
    # ========================================================================

    def _build_cobol_graph(self, state: CobolProcessingState, stats: Dict[str, int]):
        """Build graph for COBOL program"""
        parsed = state['parsed_data']
        enriched = state.get('enriched_data', {})
        source_code = state.get('file_content', '')
        program_name = parsed.get('program_name', 'UNKNOWN')

        # 1. Create program node
        self._create_program_node(parsed, enriched, source_code)
        stats["nodes_created"] += 1

        # 2. Create CALLS relationships
        for called_program in parsed.get('calls', []):
            self._create_call_relationship(program_name, called_program)
            stats["relationships_created"] += 1
            stats["nodes_created"] += 1  # Called program stub

        # 3. Create file relationships
        for file_name in parsed.get('files_read', []):
            self._create_file_relationship(program_name, file_name, "READS")
            stats["relationships_created"] += 1
            stats["nodes_created"] += 1

        for file_name in parsed.get('files_written', []):
            self._create_file_relationship(program_name, file_name, "WRITES")
            stats["relationships_created"] += 1
            stats["nodes_created"] += 1

        # 4. Create procedure nodes (limit to 20)
        for proc_name in parsed.get('procedures', [])[:20]:
            self._create_procedure_node(program_name, proc_name)
            stats["relationships_created"] += 1
            stats["nodes_created"] += 1

        # 5. Create INCLUDES relationships to copybooks
        for copybook in parsed.get('copybooks', []):
            self._create_includes_relationship(program_name, copybook)
            stats["relationships_created"] += 1

    # ========================================================================
    # COPYBOOK GRAPH BUILDER
    # ========================================================================

    def _build_copybook_graph(self, state: CobolProcessingState, stats: Dict[str, int]):
        """Build graph for Copybook"""
        copybook_data = state['copybook_data']
        copybook_name = copybook_data.get('copybook_name', 'UNKNOWN')

        # 1. Create Copybook node
        query = """
        MERGE (c:Copybook {name: $name})
        SET c.records = $records,
            c.field_count = $field_count,
            c.loc = $loc
        """
        self.client.query(query, {
            'name': copybook_name,
            'records': copybook_data.get('records', []),
            'field_count': len(copybook_data.get('fields', [])),
            'loc': copybook_data.get('loc', 0)
        })
        stats["nodes_created"] += 1

        # 2. Create DataStructure nodes for each record
        for record in copybook_data.get('records', []):
            self._create_data_structure_node(copybook_name, record)
            stats["nodes_created"] += 1
            stats["relationships_created"] += 1

        # 3. Create Field nodes (limit to top-level fields for now)
        # Full hierarchical structure could be added later
        for field in copybook_data.get('fields', [])[:50]:  # Limit to 50 fields
            if field.get('level') == '05':  # Only top-level fields
                self._create_field_node(copybook_name, field)
                stats["nodes_created"] += 1
                stats["relationships_created"] += 1

    # ========================================================================
    # JCL GRAPH BUILDER
    # ========================================================================

    def _build_jcl_graph(self, state: CobolProcessingState, stats: Dict[str, int]):
        """Build graph for JCL"""
        jcl_data = state['jcl_data']
        job_name = jcl_data.get('job_name', 'UNKNOWN')

        # 1. Create Job node
        query = """
        MERGE (j:Job {name: $name})
        SET j.step_count = $step_count,
            j.dataset_count = $dataset_count,
            j.loc = $loc
        """
        self.client.query(query, {
            'name': job_name,
            'step_count': len(jcl_data.get('steps', [])),
            'dataset_count': len(jcl_data.get('datasets', [])),
            'loc': jcl_data.get('loc', 0)
        })
        stats["nodes_created"] += 1

        # 2. Create EXECUTES relationships for each step
        for step in jcl_data.get('steps', []):
            step_name = step.get('step_name')
            program = step.get('program')
            parm = step.get('parm')

            if program:
                self._create_job_step_relationship(job_name, step_name, program, parm)
                stats["relationships_created"] += 1

        # 3. Create Dataset nodes and ALLOCATES relationships
        for dataset in jcl_data.get('datasets', []):
            ddname = dataset.get('ddname')
            dsn = dataset.get('dsn')
            ds_type = dataset.get('type')

            if dsn:
                self._create_dataset_node(job_name, ddname, dsn, ds_type)
                stats["nodes_created"] += 1
                stats["relationships_created"] += 1

    # ========================================================================
    # CICS GRAPH BUILDER
    # ========================================================================

    def _build_cics_graph(self, state: CobolProcessingState, stats: Dict[str, int]):
        """Build graph for CICS files (BMS or CSD)"""
        cics_data = state['cics_data']
        cics_type = cics_data.get('type')

        if cics_type == 'BMS':
            self._build_bms_graph(cics_data, stats)
        elif cics_type == 'CSD':
            self._build_csd_graph(cics_data, stats)

    def _build_bms_graph(self, cics_data: Dict[str, Any], stats: Dict[str, int]):
        """Build graph for BMS (screen maps)"""
        mapset_name = cics_data.get('mapset_name')

        # 1. Create Mapset node
        query = """
        MERGE (m:Mapset {name: $name})
        SET m.map_count = $map_count,
            m.loc = $loc
        """
        self.client.query(query, {
            'name': mapset_name,
            'map_count': len(cics_data.get('maps', [])),
            'loc': cics_data.get('loc', 0)
        })
        stats["nodes_created"] += 1

        # 2. Create ScreenMap nodes for each map
        for map_info in cics_data.get('maps', []):
            map_name = map_info.get('map_name')
            field_count = len(map_info.get('fields', []))

            query = """
            MERGE (s:ScreenMap {name: $name})
            SET s.mapset = $mapset,
                s.field_count = $field_count
            MERGE (m:Mapset {name: $mapset})
            MERGE (m)-[:CONTAINS_MAP]->(s)
            """
            self.client.query(query, {
                'name': map_name,
                'mapset': mapset_name,
                'field_count': field_count
            })
            stats["nodes_created"] += 1
            stats["relationships_created"] += 1

    def _build_csd_graph(self, cics_data: Dict[str, Any], stats: Dict[str, int]):
        """Build graph for CSD (CICS system definitions)"""
        # 1. Create Transaction nodes
        for trans in cics_data.get('transactions', []):
            transid = trans.get('transid')
            program = trans.get('program')
            description = trans.get('description', '')

            query = """
            MERGE (t:Transaction {transid: $transid})
            SET t.description = $description,
                t.status = $status,
                t.group = $group
            """
            self.client.query(query, {
                'transid': transid,
                'description': description,
                'status': trans.get('status', ''),
                'group': trans.get('group', '')
            })
            stats["nodes_created"] += 1

            # Create INVOKES relationship if program is specified
            if program:
                query = """
                MATCH (t:Transaction {transid: $transid})
                MERGE (p:CobolProgram {name: $program})
                MERGE (t)-[:INVOKES]->(p)
                """
                self.client.query(query, {
                    'transid': transid,
                    'program': program
                })
                stats["relationships_created"] += 1

        # 2. Update Program nodes with CICS attributes
        for prog in cics_data.get('programs', []):
            program_name = prog.get('name')
            transid = prog.get('transid')

            query = """
            MERGE (p:CobolProgram {name: $name})
            SET p.language = $language,
                p.cics_enabled = true,
                p.transid = $transid
            """
            self.client.query(query, {
                'name': program_name,
                'language': prog.get('language', ''),
                'transid': transid
            })

    # ========================================================================
    # HELPER METHODS FOR COBOL PROGRAMS
    # ========================================================================

    def _create_program_node(self, parsed: Dict[str, Any], enriched: Dict[str, Any], source_code: str = ''):
        """Create CobolProgram node with properties including source code and embedding"""

        # Limit source code size to prevent Neo4j property size issues
        # Neo4j theoretical limit: 2GB, but performance degrades above 1-2 MB
        # We truncate to 500 KB for safety and performance
        MAX_CODE_SIZE = 512000  # 500 KB (~6000-7000 lines of COBOL)

        if len(source_code) > MAX_CODE_SIZE:
            program_name = parsed.get('program_name', 'UNKNOWN')
            original_size = len(source_code)
            logger.warning(f"Source code for {program_name} is {original_size:,} bytes ({original_size/1024:.1f} KB), truncating to {MAX_CODE_SIZE/1024:.1f} KB")
            source_code = source_code[:MAX_CODE_SIZE] + "\n\n... [TRUNCATED - Original size: {:.1f} KB] ...".format(original_size/1024)

        # Check if embedding is available
        embedding = enriched.get('embedding')

        if embedding:
            # Store with embedding for semantic search
            query = """
            MERGE (p:CobolProgram {name: $name})
            SET p.author = $author,
                p.date_written = $date_written,
                p.summary = $summary,
                p.domain = $domain,
                p.complexity = $complexity,
                p.complexity_score = $complexity_score,
                p.modernization_priority = $modernization_priority,
                p.loc = $loc,
                p.code = $code,
                p.embedding = $embedding
            """
            params = {
                'name': parsed.get('program_name', 'UNKNOWN'),
                'author': parsed.get('author', ''),
                'date_written': parsed.get('date_written', ''),
                'summary': enriched.get('summary', ''),
                'domain': enriched.get('business_domain', 'Unknown'),
                'complexity': enriched.get('complexity_rating', 'Medium'),
                'complexity_score': parsed.get('complexity_score', 0),
                'modernization_priority': enriched.get('modernization_priority', 'Medium'),
                'loc': parsed.get('loc', 0),
                'code': source_code,
                'embedding': embedding
            }
        else:
            # Store without embedding (fallback for non-enriched programs)
            query = """
            MERGE (p:CobolProgram {name: $name})
            SET p.author = $author,
                p.date_written = $date_written,
                p.summary = $summary,
                p.domain = $domain,
                p.complexity = $complexity,
                p.complexity_score = $complexity_score,
                p.modernization_priority = $modernization_priority,
                p.loc = $loc,
                p.code = $code
            """
            params = {
                'name': parsed.get('program_name', 'UNKNOWN'),
                'author': parsed.get('author', ''),
                'date_written': parsed.get('date_written', ''),
                'summary': enriched.get('summary', ''),
                'domain': enriched.get('business_domain', 'Unknown'),
                'complexity': enriched.get('complexity_rating', 'Medium'),
                'complexity_score': parsed.get('complexity_score', 0),
                'modernization_priority': enriched.get('modernization_priority', 'Medium'),
                'loc': parsed.get('loc', 0),
                'code': source_code
            }

        self.client.query(query, params)

    def _create_call_relationship(self, caller: str, called: str):
        """Create CALLS relationship between programs"""
        query = """
        MATCH (caller:CobolProgram {name: $caller})
        MERGE (called:CobolProgram {name: $called})
        MERGE (caller)-[:CALLS]->(called)
        """
        self.client.query(query, {'caller': caller, 'called': called})

    def _create_file_relationship(self, program: str, file_name: str, rel_type: str):
        """Create READS/WRITES relationship to DataFile"""
        query = f"""
        MATCH (p:CobolProgram {{name: $program}})
        MERGE (f:DataFile {{name: $file}})
        MERGE (p)-[:{rel_type}]->(f)
        """
        self.client.query(query, {'program': program, 'file': file_name})

    def _create_procedure_node(self, program: str, proc_name: str):
        """Create Procedure node and relationship"""
        query = """
        MATCH (p:CobolProgram {name: $program})
        MERGE (proc:Procedure {id: $proc_id, program: $program})
        MERGE (p)-[:CONTAINS_PROCEDURE]->(proc)
        """
        self.client.query(query, {
            'program': program,
            'proc_id': proc_name
        })

    def _create_includes_relationship(self, program: str, copybook: str):
        """Create INCLUDES relationship to Copybook"""
        query = """
        MATCH (p:CobolProgram {name: $program})
        MERGE (c:Copybook {name: $copybook})
        MERGE (p)-[:INCLUDES]->(c)
        """
        self.client.query(query, {'program': program, 'copybook': copybook})

    # ========================================================================
    # HELPER METHODS FOR COPYBOOKS
    # ========================================================================

    def _create_data_structure_node(self, copybook: str, record_name: str):
        """Create DataStructure node for copybook record"""
        query = """
        MATCH (c:Copybook {name: $copybook})
        MERGE (ds:DataStructure {name: $record, copybook: $copybook})
        MERGE (c)-[:DEFINES]->(ds)
        """
        self.client.query(query, {
            'copybook': copybook,
            'record': record_name
        })

    def _create_field_node(self, copybook: str, field: Dict[str, Any]):
        """Create Field node"""
        query = """
        MATCH (c:Copybook {name: $copybook})
        MERGE (f:Field {name: $name, copybook: $copybook})
        SET f.level = $level,
            f.picture = $picture,
            f.usage = $usage
        MERGE (c)-[:HAS_FIELD]->(f)
        """
        self.client.query(query, {
            'copybook': copybook,
            'name': field.get('name'),
            'level': field.get('level'),
            'picture': field.get('picture', ''),
            'usage': field.get('usage', '')
        })

    # ========================================================================
    # HELPER METHODS FOR JCL
    # ========================================================================

    def _create_job_step_relationship(self, job: str, step_name: str, program: str, parm: str):
        """Create EXECUTES relationship from Job to Program"""
        query = """
        MATCH (j:Job {name: $job})
        MERGE (p:CobolProgram {name: $program})
        MERGE (j)-[:EXECUTES {step: $step, parm: $parm}]->(p)
        """
        self.client.query(query, {
            'job': job,
            'step': step_name,
            'program': program,
            'parm': parm or ''
        })

    def _create_dataset_node(self, job: str, ddname: str, dsn: str, ds_type: str):
        """Create Dataset node and ALLOCATES relationship"""
        query = """
        MATCH (j:Job {name: $job})
        MERGE (d:Dataset {dsn: $dsn})
        SET d.type = $type
        MERGE (j)-[:ALLOCATES {ddname: $ddname}]->(d)
        """
        self.client.query(query, {
            'job': job,
            'ddname': ddname,
            'dsn': dsn,
            'type': ds_type
        })


# Create singleton instance
graph_builder_agent = GraphBuilderAgent()


# Wrapper function for LangGraph
def graph_builder_agent_node(state: CobolProcessingState) -> CobolProcessingState:
    """LangGraph node wrapper"""
    return graph_builder_agent.process(state)
