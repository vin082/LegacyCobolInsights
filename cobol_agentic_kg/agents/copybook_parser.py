"""
Copybook Parser Agent - Extracts data structures from COBOL copybooks

Copybooks are reusable data structure definitions that are included
via COPY statements in COBOL programs. They define:
- Record structures (01 level items)
- Fields with data types (PIC clauses)
- Constants (VALUE clauses)
- Array definitions (OCCURS)
- Alternative mappings (REDEFINES)

This parser extracts these structures to enable:
- Data lineage tracking
- Impact analysis for data structure changes
- Understanding shared data across programs
"""
import re
from typing import Dict, Any, List, Tuple
from utils.state import CobolProcessingState
from utils.logger import logger


class CopybookParser:
    """Agent responsible for parsing COBOL copybook files"""

    def __init__(self):
        # Regex patterns for copybook constructs
        self.patterns = {
            # 01-level record (top-level data structure)
            # Example: "01 CUSTOMER-RECORD."
            'record': r'^\s*01\s+([\w-]+)',

            # Field definition with level number
            # Example: "   05 CUSTOMER-ID    PIC 9(10)."
            # Captures: level, field_name
            'field': r'^\s*(\d+)\s+([\w-]+)',

            # PIC (Picture) clause - defines data type
            # Examples: PIC X(20), PIC 9(5)V99, PIC S9(4) COMP-3
            'picture': r'PIC(?:TURE)?\s+([\w\d\(\)V\-]+)',

            # VALUE clause - constant/initial value
            # Examples: VALUE 'ABC', VALUE ZERO, VALUE 123
            'value': r'VALUE\s+(?:[\'"]([^\'"]+)[\'"]|(\w+))',

            # OCCURS clause - array definition
            # Example: OCCURS 50 TIMES
            'occurs': r'OCCURS\s+(\d+)(?:\s+TIMES)?',

            # REDEFINES clause - alternative data mapping
            # Example: 05 ALT-FIELD REDEFINES ORIGINAL-FIELD
            'redefines': r'REDEFINES\s+([\w-]+)',

            # COMP/USAGE clause - computational format
            # Examples: COMP, COMP-3, BINARY, PACKED-DECIMAL
            'usage': r'(?:USAGE\s+IS\s+)?(?:COMP(?:-\d)?|BINARY|PACKED-DECIMAL|DISPLAY)',

            # 88-level condition (boolean flag)
            # Example: 88 ACTIVE-STATUS VALUE 'A'
            'condition': r'^\s*88\s+([\w-]+)',
        }

    def process(self, state: CobolProcessingState) -> CobolProcessingState:
        """
        Parse copybook file and extract data structures

        Args:
            state: Current processing state with file_content

        Returns:
            Updated state with copybook_data containing:
            - copybook_name: Name of the copybook
            - records: List of record structures
            - fields: List of field definitions
            - conditions: List of 88-level conditions
        """
        file_name = state['file_metadata']['filename']
        logger.info(f"📋 COPYBOOK PARSER: Parsing {file_name}")

        try:
            content = state['file_content']

            # Strip comments (same logic as COBOL parser)
            code_only = self._strip_comments(content)

            # Extract copybook name from filename (without extension)
            copybook_name = file_name.replace('.cpy', '').replace('.CPY', '').replace('.cbl', '').replace('.CBL', '')

            # Extract all data structures
            records = self._extract_records(code_only)
            fields = self._extract_fields(code_only)
            conditions = self._extract_conditions(code_only)

            # Build hierarchical structure
            structure = self._build_hierarchical_structure(code_only)

            copybook_data = {
                "copybook_name": copybook_name,
                "records": records,  # List of 01-level record names
                "fields": fields,    # List of field definitions with metadata
                "conditions": conditions,  # List of 88-level conditions
                "structure": structure,  # Hierarchical tree of data structure
                "loc": content.count('\n'),
            }

            logger.info(f"✅ Extracted {len(records)} records, {len(fields)} fields, {len(conditions)} conditions")

            # Update state
            return {
                **state,
                "copybook_data": copybook_data,
                "stage": "copybook_parsing",
                "status": "completed"
            }

        except Exception as e:
            logger.error(f"❌ Copybook parsing failed: {e}")
            return {
                **state,
                "copybook_data": {},
                "stage": "copybook_parsing",
                "status": "failed",
                "errors": state.get('errors', []) + [f"Copybook parsing error: {str(e)}"]
            }

    def _strip_comments(self, content: str) -> str:
        """
        Remove COBOL comments (same as COBOL parser)

        Comments:
        - Lines starting with * in column 7
        - Inline comments are rare in copybooks
        """
        lines = content.split('\n')
        code_lines = []

        for line in lines:
            if not line.strip():
                code_lines.append('')
                continue

            # Check column 7 for comment marker
            if len(line) > 6 and line[6] == '*':
                code_lines.append('')
                continue

            # Also handle lines that start with *
            if line.lstrip().startswith('*'):
                code_lines.append('')
                continue

            code_lines.append(line)

        return '\n'.join(code_lines)

    def _extract_records(self, content: str) -> List[str]:
        """
        Extract 01-level record names

        Example:
            01 CUSTOMER-RECORD.
            01 ORDER-HEADER.

        Returns:
            ['CUSTOMER-RECORD', 'ORDER-HEADER']
        """
        records = re.findall(self.patterns['record'], content, re.MULTILINE | re.IGNORECASE)
        return list(set(records))

    def _extract_conditions(self, content: str) -> List[Dict[str, str]]:
        """
        Extract 88-level condition names

        Example:
            88 ACTIVE-STATUS VALUE 'A'.
            88 INACTIVE-STATUS VALUE 'I'.

        Returns:
            [
                {'name': 'ACTIVE-STATUS', 'value': 'A'},
                {'name': 'INACTIVE-STATUS', 'value': 'I'}
            ]
        """
        conditions = []
        lines = content.split('\n')

        for line in lines:
            # Match 88-level
            condition_match = re.search(self.patterns['condition'], line, re.IGNORECASE)
            if condition_match:
                cond_name = condition_match.group(1)

                # Extract VALUE
                value_match = re.search(self.patterns['value'], line, re.IGNORECASE)
                if value_match:
                    # VALUE can be quoted string or keyword
                    value = value_match.group(1) or value_match.group(2)
                else:
                    value = None

                conditions.append({
                    'name': cond_name,
                    'value': value
                })

        return conditions

    def _extract_fields(self, content: str) -> List[Dict[str, Any]]:
        """
        Extract field definitions with metadata

        Example:
            05 CUSTOMER-ID    PIC 9(10).
            05 CUSTOMER-NAME  PIC X(40).
            05 BALANCE        PIC S9(11)V99 COMP-3.

        Returns:
            [
                {
                    'level': '05',
                    'name': 'CUSTOMER-ID',
                    'picture': '9(10)',
                    'usage': 'DISPLAY',
                    'value': None,
                    'occurs': None,
                    'redefines': None
                },
                ...
            ]
        """
        fields = []
        lines = content.split('\n')

        for line in lines:
            # Match field definition
            field_match = re.search(self.patterns['field'], line, re.IGNORECASE)
            if not field_match:
                continue

            level = field_match.group(1)
            field_name = field_match.group(2)

            # Skip 01-level (those are records) and 88-level (conditions)
            if level in ['01', '88']:
                continue

            # Extract PIC clause
            pic_match = re.search(self.patterns['picture'], line, re.IGNORECASE)
            picture = pic_match.group(1) if pic_match else None

            # Extract VALUE
            value_match = re.search(self.patterns['value'], line, re.IGNORECASE)
            value = None
            if value_match:
                value = value_match.group(1) or value_match.group(2)

            # Extract OCCURS
            occurs_match = re.search(self.patterns['occurs'], line, re.IGNORECASE)
            occurs = int(occurs_match.group(1)) if occurs_match else None

            # Extract REDEFINES
            redefines_match = re.search(self.patterns['redefines'], line, re.IGNORECASE)
            redefines = redefines_match.group(1) if redefines_match else None

            # Extract USAGE
            usage_match = re.search(self.patterns['usage'], line, re.IGNORECASE)
            usage = usage_match.group(0) if usage_match else 'DISPLAY'

            fields.append({
                'level': level,
                'name': field_name,
                'picture': picture,
                'usage': usage,
                'value': value,
                'occurs': occurs,
                'redefines': redefines
            })

        return fields

    def _build_hierarchical_structure(self, content: str) -> List[Dict[str, Any]]:
        """
        Build hierarchical tree of data structure based on level numbers

        Example:
            01 CUSTOMER-RECORD.
               05 CUSTOMER-ID    PIC 9(10).
               05 ADDRESS.
                  10 STREET      PIC X(30).
                  10 CITY        PIC X(20).

        Returns:
            [
                {
                    'level': '01',
                    'name': 'CUSTOMER-RECORD',
                    'children': [
                        {'level': '05', 'name': 'CUSTOMER-ID', 'picture': '9(10)', 'children': []},
                        {
                            'level': '05',
                            'name': 'ADDRESS',
                            'children': [
                                {'level': '10', 'name': 'STREET', 'picture': 'X(30)', 'children': []},
                                {'level': '10', 'name': 'CITY', 'picture': 'X(20)', 'children': []}
                            ]
                        }
                    ]
                }
            ]
        """
        structure = []
        stack = []  # Stack to track hierarchy: [(level, node), ...]

        lines = content.split('\n')

        for line in lines:
            # Match field/record definition
            field_match = re.search(self.patterns['field'], line, re.IGNORECASE)
            if not field_match:
                continue

            level = int(field_match.group(1))
            name = field_match.group(2)

            # Extract PIC if present
            pic_match = re.search(self.patterns['picture'], line, re.IGNORECASE)
            picture = pic_match.group(1) if pic_match else None

            # Create node
            node = {
                'level': str(level),
                'name': name,
                'picture': picture,
                'children': []
            }

            # Pop stack until we find the parent level
            while stack and stack[-1][0] >= level:
                stack.pop()

            # Add to parent's children or root
            if stack:
                parent_level, parent_node = stack[-1]
                parent_node['children'].append(node)
            else:
                structure.append(node)

            # Push current node to stack
            stack.append((level, node))

        return structure


# Create singleton instance
copybook_parser = CopybookParser()


# Wrapper function for LangGraph
def copybook_parser_node(state: CobolProcessingState) -> CobolProcessingState:
    """LangGraph node wrapper"""
    return copybook_parser.process(state)
