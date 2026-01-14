"""
Shared state definitions for all agents
"""
from typing import TypedDict, Annotated, Sequence, Optional, List, Dict, Any
import operator


class CobolProcessingState(TypedDict):
    """
    Shared state across all agents in the workflow

    This state is passed between agents and accumulates information
    as the COBOL file progresses through the processing pipeline.
    """

    # ========================================================================
    # File Information
    # ========================================================================
    file_id: str  # Unique identifier for this file
    file_path: str  # Path to COBOL file
    file_content: str  # Raw COBOL code
    file_metadata: Dict[str, Any]  # Metadata (size, encoding, etc.)

    # ========================================================================
    # Processing Status
    # ========================================================================
    stage: str  # Current processing stage
    status: str  # pending, processing, completed, failed
    errors: Annotated[List[str], operator.add]  # Accumulated errors

    # ========================================================================
    # Agent Outputs
    # ========================================================================
    # Validation agent output
    is_valid: bool
    file_type: str  # COBOL_PROGRAM, COPYBOOK, JCL, etc.

    # Parsing agent output
    parsed_data: Dict[str, Any]  # Extracted entities

    # Enrichment agent output
    enriched_data: Dict[str, Any]  # LLM-generated insights

    # Graph builder output
    graph_data: Dict[str, Any]  # Neo4j metrics

    # ========================================================================
    # Query Handling (for interactive queries)
    # ========================================================================
    user_query: Optional[str]  # Natural language query
    generated_cypher: Optional[str]  # Generated Cypher query
    query_results: List[Dict[str, Any]]  # Query results
    answer: Optional[str]  # Natural language answer

    # ========================================================================
    # Metrics
    # ========================================================================
    processing_time: float  # Total processing time in seconds
    tokens_used: int  # LLM tokens consumed
    timestamp: str  # ISO timestamp


class BatchProcessingState(TypedDict):
    """
    State for batch processing multiple COBOL files
    """

    # Batch information
    batch_id: str
    repository_url: Optional[str]
    total_files: int

    # Processing status
    files_queued: List[str]
    files_processing: List[str]
    files_completed: List[str]
    files_failed: List[Dict[str, Any]]

    # Aggregate metrics
    total_programs: int
    total_relationships: int
    total_nodes: int

    # Progress tracking
    progress_percentage: float
    estimated_time_remaining: float

    # Errors and warnings
    errors: Annotated[List[str], operator.add]
    warnings: Annotated[List[str], operator.add]


class DocumentGenerationState(TypedDict):
    """
    State for document generation agent
    """

    # Document configuration
    doc_type: str  # system_overview, program_detail, dependency_map, etc.
    format: str  # markdown, word, pdf
    filters: Dict[str, Any]  # Optional filters (domain, complexity, etc.)

    # Processing status
    stage: str  # document_generation
    status: str  # pending, processing, completed, failed
    errors: Annotated[List[str], operator.add]

    # Output
    file_path: Optional[str]  # Path to generated document
    generation_time: float  # Time taken to generate document
    timestamp: str  # ISO timestamp


class ModernizationState(TypedDict):
    """
    State for modernization analysis agent
    """

    # Analysis configuration
    filters: Dict[str, Any]  # Optional filters (max_programs, complexity, domain)

    # Processing status
    status: str  # pending, analyzing, completed, failed
    errors: Annotated[List[str], operator.add]

    # Output - list of recommendations
    recommendations: List[Dict[str, Any]]  # Each recommendation contains:
    # - program_name: str
    # - domain: str
    # - complexity: int
    # - loc: int
    # - risk_score: float (0-100)
    # - value_score: float (0-100)
    # - priority_score: float (0-100)
    # - strategy: str (Rewrite, Strangler Fig, Retire/Replace, Encapsulate)
    # - risk_factors: List[str]
    # - value_factors: List[str]
    # - recommended_approach: str
    # - technology_recommendations: List[str]
    # - estimated_effort: str
    # - key_considerations: List[str]

    # Metrics
    analysis_time: float  # Time taken to analyze programs
    timestamp: str  # ISO timestamp


class CodeTranslationState(TypedDict):
    """
    State for code translation agent
    """

    # Input configuration
    program_names: List[str]  # Programs to translate (can be multiple)
    target_language: str  # "java", "python", "csharp"
    target_framework: str  # "spring-boot", "fastapi", "dotnet-core"

    # Options
    include_tests: bool  # Generate test stubs
    include_comments: bool  # Preserve COBOL comments
    package_name: Optional[str]  # Optional: com.example.cobol for Java

    # Processing status
    status: str  # pending, translating, completed, failed
    errors: Annotated[List[str], operator.add]

    # Output - list of translations
    translations: List[Dict[str, Any]]  # Each translation contains:
    # {
    #   'program_name': str,
    #   'target_language': str,
    #   'main_file_path': str,
    #   'test_file_path': Optional[str],
    #   'readme_path': str,
    #   'main_code': str,
    #   'test_code': Optional[str],
    #   'conversion_notes': List[str],
    #   'manual_review_items': List[str],
    #   'translation_success': bool,
    #   'error_message': Optional[str]
    # }

    # Metrics
    translation_time: float  # Time taken to translate all programs
    timestamp: str  # ISO timestamp


def create_initial_state(file_path: str, file_id: Optional[str] = None) -> CobolProcessingState:
    """
    Create initial state for a COBOL file

    Args:
        file_path: Path to the COBOL file
        file_id: Optional unique identifier

    Returns:
        Initial state dictionary
    """
    import uuid
    from datetime import datetime

    return CobolProcessingState(
        file_id=file_id or str(uuid.uuid4()),
        file_path=file_path,
        file_content="",
        file_metadata={},
        stage="ingestion",
        status="pending",
        errors=[],
        is_valid=False,
        file_type="UNKNOWN",
        parsed_data={},
        enriched_data={},
        graph_data={},
        user_query=None,
        generated_cypher=None,
        query_results=[],
        answer=None,
        processing_time=0.0,
        tokens_used=0,
        timestamp=datetime.utcnow().isoformat()
    )


def create_batch_state(repository_url: Optional[str] = None) -> BatchProcessingState:
    """
    Create initial state for batch processing

    Args:
        repository_url: Optional GitHub repository URL

    Returns:
        Initial batch state dictionary
    """
    import uuid

    return BatchProcessingState(
        batch_id=str(uuid.uuid4()),
        repository_url=repository_url,
        total_files=0,
        files_queued=[],
        files_processing=[],
        files_completed=[],
        files_failed=[],
        total_programs=0,
        total_relationships=0,
        total_nodes=0,
        progress_percentage=0.0,
        estimated_time_remaining=0.0,
        errors=[],
        warnings=[]
    )
