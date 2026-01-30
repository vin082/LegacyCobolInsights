"""
Validation Agent - Validates mainframe artifact structure and syntax

Supports:
- COBOL programs (.cbl, .cob, .cobol)
- Copybooks (.cpy)
- JCL files (.jcl)
- BMS screen maps (.bms)
- CSD system definitions (.csd)
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
        Detect mainframe file type

        Args:
            content: File content

        Returns:
            File type (COBOL_PROGRAM, COPYBOOK, JCL, BMS, CSD, UNKNOWN)
        """
        content_upper = content.upper()

        # Check for JCL indicators first (most distinctive)
        # JCL starts with // in columns 1-2
        if content.startswith('//') or (content.lstrip().startswith('//') and ' JOB ' in content_upper[:200]):
            return "JCL"

        # Check for BMS (CICS screen maps)
        if 'DFHMSD' in content_upper or 'DFHMDI' in content_upper:
            return "BMS"

        # Check for CSD (CICS system definitions)
        if 'DEFINE TRANSACTION' in content_upper or 'DEFINE PROGRAM' in content_upper:
            return "CSD"

        # Check for PROGRAM-ID (COBOL program)
        if 'PROGRAM-ID' in content_upper:
            return "COBOL_PROGRAM"

        # Check for COPY statement or 01-level (copybook)
        # Copybooks typically have data structures but no PROGRAM-ID
        if re.search(r'\bCOPY\b', content_upper) or (re.search(r'^\s*01\s+\w+', content, re.MULTILINE) and 'PROGRAM-ID' not in content_upper):
            return "COPYBOOK"

        # Check if it has COBOL divisions
        if any(div in content_upper for div in self.required_divisions):
            return "COBOL_PROGRAM"

        return "UNKNOWN"

    def _validate_structure(self, content: str, file_type: str) -> Tuple[bool, List[str]]:
        """
        Validate mainframe file structure

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
            # Copybooks have lighter requirements - just check for data definitions
            if not re.search(r'\d{2}\s+\w+', content):
                errors.append("Copybook appears to have invalid structure")

        elif file_type == "JCL":
            # JCL validation - check for job card
            if not re.search(r'^//\w+\s+JOB\s+', content, re.MULTILINE):
                errors.append("JCL file missing JOB card")

        elif file_type == "BMS":
            # BMS validation - check for mapset definition
            content_upper = content.upper()
            if 'DFHMSD' not in content_upper:
                errors.append("BMS file missing DFHMSD (mapset definition)")

        elif file_type == "CSD":
            # CSD validation - check for DEFINE statements
            content_upper = content.upper()
            if not re.search(r'DEFINE\s+(TRANSACTION|PROGRAM)', content_upper):
                errors.append("CSD file missing DEFINE statements")

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
