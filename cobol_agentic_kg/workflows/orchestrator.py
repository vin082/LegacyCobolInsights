"""
Orchestrator - Main workflow coordinator using LangGraph

Workflow Paths:
1. File Processing: ingestion → validation → parsing → enrichment → graph_builder
2. Query Path: cypher_generator → retrieval
3. Document Generation: Standalone (operates on KG)
4. Modernization Analysis: Standalone (operates on KG)
5. Code Translation: Standalone (operates on KG)

Note: Document Generator, Modernization Agent, and Code Translation Agent are
standalone agents that query the knowledge graph directly. They are invoked
independently from the UI rather than as part of the file processing pipeline.
"""
from langgraph.graph import StateGraph, END
from utils.state import CobolProcessingState, create_initial_state
from agents.ingestion import ingestion_agent_node
from agents.validation import validation_agent_node
from agents.parsing import parsing_agent_node
from agents.enrichment import enrichment_agent_node
from agents.graph_builder import graph_builder_agent_node
from agents.cypher_gen import cypher_generator_agent_node
from agents.retrieval import retrieval_agent_node
# Note: document_generator, modernization, and translation are standalone agents
# They are imported and used directly in ui/app.py, not in the workflow
from utils.logger import logger


class CobolWorkflowOrchestrator:
    """Orchestrates the multi-agent COBOL processing workflow"""

    def __init__(self):
        self.workflow = self._build_workflow()

    def _build_workflow(self) -> StateGraph:
        """
        Build the LangGraph workflow for COBOL file processing

        This workflow handles the core file processing pipeline:
        - Ingestion: Load COBOL files
        - Validation: Check COBOL syntax
        - Parsing: Extract program structure
        - Enrichment: Add LLM insights
        - Graph Building: Create Neo4j nodes/relationships
        - Query: Optional natural language queries

        Note: Document Generation, Modernization Analysis, and Code Translation
        are NOT part of this workflow. They are standalone agents that operate
        on the populated knowledge graph and are invoked directly from the UI.
        """

        workflow = StateGraph(CobolProcessingState)

        # Add agent nodes for file processing pipeline
        workflow.add_node("ingestion", ingestion_agent_node)
        workflow.add_node("validation", validation_agent_node)
        workflow.add_node("parsing", parsing_agent_node)
        workflow.add_node("enrichment", enrichment_agent_node)
        workflow.add_node("graph_builder", graph_builder_agent_node)
        workflow.add_node("cypher_generator", cypher_generator_agent_node)
        workflow.add_node("retrieval", retrieval_agent_node)

        # Define workflow edges
        workflow.set_entry_point("ingestion")
        workflow.add_edge("ingestion", "validation")

        # Conditional: continue only if validation passes
        workflow.add_conditional_edges(
            "validation",
            self._should_continue_after_validation,
            {
                "continue": "parsing",
                "end": END
            }
        )

        workflow.add_edge("parsing", "enrichment")
        workflow.add_edge("enrichment", "graph_builder")

        # Conditional: query path or end
        workflow.add_conditional_edges(
            "graph_builder",
            self._route_after_graph_build,
            {
                "query": "cypher_generator",
                "end": END
            }
        )

        workflow.add_edge("cypher_generator", "retrieval")
        workflow.add_edge("retrieval", END)

        return workflow.compile()

    def _should_continue_after_validation(self, state: CobolProcessingState) -> str:
        """Decide whether to continue or stop based on validation"""
        if not state.get('is_valid', False):
            logger.warning(f"Validation failed for {state['file_path']}")
            return 'end'
        return 'continue'

    def _route_after_graph_build(self, state: CobolProcessingState) -> str:
        """After graph building, decide next step"""
        if state.get('user_query'):
            return 'query'
        return 'end'

    def process_file(self, file_path: str, user_query: str = None) -> CobolProcessingState:
        """
        Process a single COBOL file

        Args:
            file_path: Path to COBOL file
            user_query: Optional query to execute after processing

        Returns:
            Final state after processing
        """
        logger.info(f"="*80)
        logger.info(f"Processing: {file_path}")
        logger.info(f"="*80)

        # Create initial state
        initial_state = create_initial_state(file_path)
        if user_query:
            initial_state['user_query'] = user_query

        # Run workflow
        final_state = self.workflow.invoke(initial_state)

        logger.info(f"Processing complete. Status: {final_state['status']}")
        return final_state

    def process_batch(self, file_paths: list, progress_callback=None) -> list:
        """
        Process multiple COBOL files in batch

        Args:
            file_paths: List of file paths
            progress_callback: Optional callback for progress updates

        Returns:
            List of final states
        """
        results = []
        total = len(file_paths)

        for i, file_path in enumerate(file_paths, 1):
            logger.info(f"\nProcessing file {i}/{total}: {file_path}")

            try:
                result = self.process_file(file_path)
                results.append(result)

                if progress_callback:
                    progress_callback(i, total, result)

            except Exception as e:
                logger.error(f"Failed to process {file_path}: {e}")
                results.append({
                    'file_path': file_path,
                    'status': 'failed',
                    'errors': [str(e)]
                })

        return results

    def query_graph(self, user_query: str) -> CobolProcessingState:
        """
        Execute a query against the existing knowledge graph

        Args:
            user_query: Natural language query

        Returns:
            State with query results
        """
        logger.info(f"Executing query: {user_query}")

        # Create minimal state for query-only workflow
        query_state = CobolProcessingState(
            file_id="query",
            file_path="",
            file_content="",
            file_metadata={},
            stage="query",
            status="pending",
            errors=[],
            is_valid=True,
            file_type="QUERY",
            parsed_data={},
            enriched_data={},
            graph_data={},
            user_query=user_query,
            generated_cypher="",
            query_results=[],
            processing_time=0.0,
            tokens_used=0,
            timestamp=""
        )

        # Build query-only workflow
        query_workflow = StateGraph(CobolProcessingState)
        query_workflow.add_node("cypher_generator", cypher_generator_agent_node)
        query_workflow.add_node("retrieval", retrieval_agent_node)
        query_workflow.set_entry_point("cypher_generator")
        query_workflow.add_edge("cypher_generator", "retrieval")
        query_workflow.add_edge("retrieval", END)

        compiled = query_workflow.compile()
        return compiled.invoke(query_state)


# Global orchestrator instance
orchestrator = CobolWorkflowOrchestrator()
