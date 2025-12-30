"""
Parsing Agent - Extracts structured data from COBOL code
"""
import re
from typing import Dict, Any, List, Set
from utils.state import CobolProcessingState
from utils.logger import logger


class ParsingAgent:
    """Agent responsible for parsing COBOL code"""

    def __init__(self):
        # Regex patterns for COBOL constructs
        self.patterns = {
            'program_id': r'PROGRAM-ID\.\s+(\w+)',
            'author': r'AUTHOR\.\s+(.+)',
            'date_written': r'DATE-WRITTEN\.\s+(.+)',
            'call': r'CALL\s+[\'"](\w+)[\'"]',
            'file_read': r'READ\s+(\w+)',
            'file_write': r'WRITE\s+(\w+)',
            'file_select': r'SELECT\s+(\w+)\s+ASSIGN',
            'procedure': r'^\s{7,}(\w+(?:-\w+)*)\.',
            'variable': r'^\s+\d+\s+(\w+(?:-\w+)*)\s+PIC',
            'copybook': r'COPY\s+(\w+)',
        }

    def process(self, state: CobolProcessingState) -> CobolProcessingState:
        """
        Parse COBOL code and extract entities

        Args:
            state: Current processing state

        Returns:
            Updated state with parsed data
        """
        program_name = state['file_metadata']['filename']
        logger.info(f"🔍 PARSING AGENT: Parsing {program_name}")

        try:
            content = state['file_content']

            # Extract all entities
            parsed_data = {
                "program_name": self._extract_program_name(content),
                "author": self._extract_author(content),
                "date_written": self._extract_date_written(content),
                "calls": self._extract_calls(content),
                "files_read": self._extract_files_read(content),
                "files_written": self._extract_files_written(content),
                "files_declared": self._extract_files_declared(content),
                "procedures": self._extract_procedures(content),
                "variables": self._extract_variables(content),
                "copybooks": self._extract_copybooks(content),
                "loc": content.count('\n'),
                "complexity_score": self._calculate_complexity(content)
            }

            # Update state
            return {
                **state,
                "parsed_data": parsed_data,
                "stage": "parsing",
                "status": "completed"
            }

        except Exception as e:
            logger.error(f"Parsing failed: {e}")
            return {
                **state,
                "parsed_data": {},
                "stage": "parsing",
                "status": "failed",
                "errors": state.get('errors', []) + [f"Parsing error: {str(e)}"]
            }

    def _extract_program_name(self, content: str) -> str:
        """Extract PROGRAM-ID"""
        match = re.search(self.patterns['program_id'], content, re.IGNORECASE)
        return match.group(1) if match else "UNKNOWN"

    def _extract_author(self, content: str) -> str:
        """Extract AUTHOR"""
        match = re.search(self.patterns['author'], content, re.IGNORECASE)
        return match.group(1).strip() if match else ""

    def _extract_date_written(self, content: str) -> str:
        """Extract DATE-WRITTEN"""
        match = re.search(self.patterns['date_written'], content, re.IGNORECASE)
        return match.group(1).strip() if match else ""

    def _extract_calls(self, content: str) -> List[str]:
        """Extract CALL statements"""
        return list(set(re.findall(self.patterns['call'], content, re.IGNORECASE)))

    def _extract_files_read(self, content: str) -> List[str]:
        """Extract READ operations"""
        return list(set(re.findall(self.patterns['file_read'], content, re.IGNORECASE)))

    def _extract_files_written(self, content: str) -> List[str]:
        """Extract WRITE operations"""
        return list(set(re.findall(self.patterns['file_write'], content, re.IGNORECASE)))

    def _extract_files_declared(self, content: str) -> List[str]:
        """Extract SELECT file declarations"""
        return list(set(re.findall(self.patterns['file_select'], content, re.IGNORECASE)))

    def _extract_procedures(self, content: str) -> List[str]:
        """Extract procedure/paragraph names"""
        procedures = re.findall(self.patterns['procedure'], content, re.MULTILINE | re.IGNORECASE)
        # Filter out DIVISION names and limit to 50
        filtered = [p for p in procedures if 'DIVISION' not in p.upper()]
        return list(set(filtered))[:50]

    def _extract_variables(self, content: str) -> List[str]:
        """Extract variable declarations"""
        variables = re.findall(self.patterns['variable'], content, re.MULTILINE | re.IGNORECASE)
        return list(set(variables))[:100]  # Limit to 100

    def _extract_copybooks(self, content: str) -> List[str]:
        """Extract COPY statements (copybook inclusions)"""
        return list(set(re.findall(self.patterns['copybook'], content, re.IGNORECASE)))

    def _calculate_complexity(self, content: str) -> int:
        """
        Calculate code complexity score (simplified McCabe)

        Returns:
            Complexity score (1-100)
        """
        # Count decision points
        decision_keywords = ['IF', 'EVALUATE', 'PERFORM', 'GOTO']
        complexity = 1  # Base complexity

        content_upper = content.upper()
        for keyword in decision_keywords:
            complexity += content_upper.count(keyword)

        # Normalize to 1-100 scale
        return min(100, complexity)


# Create singleton instance
parsing_agent = ParsingAgent()


# Wrapper function for LangGraph
def parsing_agent_node(state: CobolProcessingState) -> CobolProcessingState:
    """LangGraph node wrapper"""
    return parsing_agent.process(state)
