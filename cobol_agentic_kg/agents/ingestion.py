"""
Ingestion Agent - Handles file uploads and repository scanning
"""
import os
from typing import Dict, Any
from utils.state import CobolProcessingState
from utils.logger import logger
import chardet


class IngestionAgent:
    """Agent responsible for ingesting COBOL files"""

    def __init__(self):
        # Supported file extensions for mainframe artifacts
        self.supported_extensions = [
            '.cob', '.cbl', '.cobol',  # COBOL programs
            '.cpy',                     # Copybooks
            '.jcl',                     # JCL (Job Control Language)
            '.bms',                     # BMS (CICS screen maps)
            '.csd',                     # CSD (CICS system definitions)
        ]

    def process(self, state: CobolProcessingState) -> CobolProcessingState:
        """
        Ingest COBOL file and extract metadata

        Args:
            state: Current processing state

        Returns:
            Updated state with file content and metadata
        """
        logger.info(f"🔽 INGESTION AGENT: Processing {state['file_path']}")

        try:
            # Read file with encoding detection
            file_content = self._read_file(state['file_path'])

            # Create metadata
            metadata = self._create_metadata(state['file_path'], file_content)

            # Update state
            return {
                **state,
                "file_content": file_content,
                "file_metadata": metadata,
                "stage": "ingestion",
                "status": "completed"
            }

        except Exception as e:
            logger.error(f"Ingestion failed: {e}")
            return {
                **state,
                "stage": "ingestion",
                "status": "failed",
                "errors": state.get('errors', []) + [f"Ingestion error: {str(e)}"]
            }

    def _read_file(self, file_path: str) -> str:
        """
        Read file with automatic encoding detection

        Args:
            file_path: Path to file

        Returns:
            File content as string
        """
        # Try UTF-8 first
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                return f.read()
        except UnicodeDecodeError:
            # Detect encoding
            with open(file_path, 'rb') as f:
                raw_data = f.read()
                result = chardet.detect(raw_data)
                encoding = result['encoding'] or 'utf-8'

            # Read with detected encoding
            with open(file_path, 'r', encoding=encoding, errors='ignore') as f:
                return f.read()

    def _create_metadata(self, file_path: str, content: str) -> Dict[str, Any]:
        """
        Create file metadata

        Args:
            file_path: Path to file
            content: File content

        Returns:
            Metadata dictionary
        """
        return {
            "filename": os.path.basename(file_path),
            "directory": os.path.dirname(file_path),
            "size_bytes": len(content.encode('utf-8')),
            "line_count": content.count('\n'),
            "char_count": len(content),
            "extension": os.path.splitext(file_path)[1]
        }

    def is_cobol_file(self, file_path: str) -> bool:
        """
        Check if file is a COBOL file by extension

        Args:
            file_path: Path to file

        Returns:
            True if COBOL file, False otherwise
        """
        ext = os.path.splitext(file_path)[1].lower()
        return ext in self.supported_extensions


# Create singleton instance
ingestion_agent = IngestionAgent()


# Wrapper function for LangGraph
def ingestion_agent_node(state: CobolProcessingState) -> CobolProcessingState:
    """LangGraph node wrapper"""
    return ingestion_agent.process(state)
