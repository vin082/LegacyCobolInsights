"""
Graph Builder Agent - Constructs Neo4j knowledge graph
"""
from typing import Dict, Any
from utils.state import CobolProcessingState
from utils.neo4j_client import neo4j_client
from utils.logger import logger


class GraphBuilderAgent:
    """Agent responsible for building the knowledge graph"""

    def __init__(self):
        self.client = neo4j_client

    def process(self, state: CobolProcessingState) -> CobolProcessingState:
        """
        Build knowledge graph from parsed and enriched data

        Args:
            state: Current processing state

        Returns:
            Updated state with graph statistics
        """
        program_name = state['parsed_data'].get('program_name', 'UNKNOWN')
        logger.info(f"🕸️  GRAPH BUILDER AGENT: Creating graph for {program_name}")

        try:
            parsed = state['parsed_data']
            enriched = state['enriched_data']

            # Track created nodes and relationships
            stats = {
                "nodes_created": 0,
                "relationships_created": 0
            }

            # 1. Create program node
            self._create_program_node(parsed, enriched)
            stats["nodes_created"] += 1

            # 2. Create CALLS relationships
            for called_program in parsed.get('calls', []):
                self._create_call_relationship(program_name, called_program)
                stats["relationships_created"] += 1
                stats["nodes_created"] += 1  # Called program node

            # 3. Create file relationships
            for file_name in parsed.get('files_read', []):
                self._create_file_relationship(program_name, file_name, "READS")
                stats["relationships_created"] += 1
                stats["nodes_created"] += 1

            for file_name in parsed.get('files_written', []):
                self._create_file_relationship(program_name, file_name, "WRITES")
                stats["relationships_created"] += 1
                stats["nodes_created"] += 1

            # 4. Create procedure nodes
            for proc_name in parsed.get('procedures', [])[:20]:  # Limit to 20
                self._create_procedure_node(program_name, proc_name)
                stats["relationships_created"] += 1
                stats["nodes_created"] += 1

            # Update state
            return {
                **state,
                "graph_data": stats,
                "stage": "graph_building",
                "status": "completed"
            }

        except Exception as e:
            logger.error(f"Graph building failed: {e}")
            return {
                **state,
                "graph_data": {"nodes_created": 0, "relationships_created": 0},
                "stage": "graph_building",
                "status": "failed",
                "errors": state.get('errors', []) + [f"Graph building error: {str(e)}"]
            }

    def _create_program_node(self, parsed: Dict[str, Any], enriched: Dict[str, Any]):
        """Create CobolProgram node with properties"""
        query = """
        MERGE (p:CobolProgram {name: $name})
        SET p.author = $author,
            p.date_written = $date_written,
            p.summary = $summary,
            p.domain = $domain,
            p.complexity = $complexity,
            p.complexity_score = $complexity_score,
            p.modernization_priority = $modernization_priority,
            p.loc = $loc
        """

        self.client.query(query, {
            'name': parsed.get('program_name', 'UNKNOWN'),
            'author': parsed.get('author', ''),
            'date_written': parsed.get('date_written', ''),
            'summary': enriched.get('summary', ''),
            'domain': enriched.get('business_domain', 'Unknown'),
            'complexity': enriched.get('complexity_rating', 'Medium'),
            'complexity_score': parsed.get('complexity_score', 0),
            'modernization_priority': enriched.get('modernization_priority', 'Medium'),
            'loc': parsed.get('loc', 0)
        })

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


# Create singleton instance
graph_builder_agent = GraphBuilderAgent()


# Wrapper function for LangGraph
def graph_builder_agent_node(state: CobolProcessingState) -> CobolProcessingState:
    """LangGraph node wrapper"""
    return graph_builder_agent.process(state)
