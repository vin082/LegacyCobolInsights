"""
Validation Agent - Validates COBOL file structure and syntax
"""
import re
from typing import List, Tuple
from utils.state import CobolProcessingState
from utils.logger import logger


class ValidationAgent:
    """Agent responsible for validating COBOL files"""

    def __init__(self):
        self.required_divisions = ['IDENTIFICATION DIVISION', 'PROCEDURE DIVISION']
        self.optional_divisions = ['ENVIRONMENT DIVISION', 'DATA DIVISION']

    def process(self, state: CobolProcessingState) -> CobolProcessingState:
        """
        Validate COBOL file structure

        Args:
            state: Current processing state

        Returns:
            Updated state with validation results
        """
        logger.info(f"✅ VALIDATION AGENT: Validating {state['file_metadata']['filename']}")

        content = state['file_content']
        errors = []

        try:
            # Determine file type
            file_type = self._detect_file_type(content)

            # Run validation checks
            is_valid, validation_errors = self._validate_structure(content, file_type)

            errors.extend(validation_errors)

            # Update state
            return {
                **state,
                "is_valid": is_valid,
                "file_type": file_type,
                "stage": "validation",
                "status": "completed" if is_valid else "failed",
                "errors": state.get('errors', []) + errors
            }

        except Exception as e:
            logger.error(f"Validation failed: {e}")
            return {
                **state,
                "is_valid": False,
                "file_type": "UNKNOWN",
                "stage": "validation",
                "status": "failed",
                "errors": state.get('errors', []) + [f"Validation error: {str(e)}"]
            }

    def _detect_file_type(self, content: str) -> str:
        """
        Detect COBOL file type

        Args:
            content: File content

        Returns:
            File type (COBOL_PROGRAM, COPYBOOK, JCL, UNKNOWN)
        """
        content_upper = content.upper()

        # Check for PROGRAM-ID (COBOL program)
        if 'PROGRAM-ID' in content_upper:
            return "COBOL_PROGRAM"

        # Check for COPY statement (copybook)
        if re.search(r'\bCOPY\b', content_upper):
            return "COPYBOOK"

        # Check for JCL indicators
        if content.startswith('//') or 'JOB' in content_upper[:100]:
            return "JCL"

        # Check if it has COBOL divisions
        if any(div in content_upper for div in self.required_divisions):
            return "COBOL_PROGRAM"

        return "UNKNOWN"

    def _validate_structure(self, content: str, file_type: str) -> Tuple[bool, List[str]]:
        """
        Validate COBOL structure

        Args:
            content: File content
            file_type: Detected file type

        Returns:
            Tuple of (is_valid, error_messages)
        """
        errors = []

        if file_type == "COBOL_PROGRAM":
            # Check for required divisions
            for division in self.required_divisions:
                if division not in content.upper():
                    errors.append(f"Missing {division}")

            # Check for PROGRAM-ID
            if not re.search(r'PROGRAM-ID\.\s+\w+', content, re.IGNORECASE):
                errors.append("Missing or invalid PROGRAM-ID")

            # Check for basic COBOL syntax
            if not self._has_valid_cobol_syntax(content):
                errors.append("Invalid COBOL syntax detected")

        elif file_type == "COPYBOOK":
            # Copybooks have lighter requirements
            if not re.search(r'\d{2}\s+\w+', content):
                errors.append("Copybook appears to have invalid structure")

        elif file_type == "JCL":
            errors.append("JCL files are not supported (COBOL files only)")

        elif file_type == "UNKNOWN":
            errors.append("Could not determine file type")

        is_valid = len(errors) == 0
        return is_valid, errors

    def _has_valid_cobol_syntax(self, content: str) -> bool:
        """
        Check for basic COBOL syntax validity

        Args:
            content: File content

        Returns:
            True if basic syntax checks pass
        """
        # Check for common COBOL keywords
        cobol_keywords = [
            'MOVE', 'IF', 'PERFORM', 'DISPLAY', 'ACCEPT',
            'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
            'OPEN', 'CLOSE', 'READ', 'WRITE'
        ]

        content_upper = content.upper()
        has_keywords = any(keyword in content_upper for keyword in cobol_keywords)

        # Check for period-terminated statements (COBOL requirement)
        has_periods = '.' in content

        return has_keywords and has_periods


# Create singleton instance
validation_agent = ValidationAgent()


# Wrapper function for LangGraph
def validation_agent_node(state: CobolProcessingState) -> CobolProcessingState:
    """LangGraph node wrapper"""
    return validation_agent.process(state)
