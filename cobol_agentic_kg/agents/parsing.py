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
            # Match PROGRAM-ID - improved pattern
            # After line numbers are stripped, pattern is simpler:
            # "PROGRAM-ID. MYPROGRAM" or "PROGRAM-ID.\nMYPROGRAM"
            'program_id': r'PROGRAM-ID\.\s*([\w-]+)',
            'author': r'AUTHOR\.\s+(.+)',
            'date_written': r'DATE-WRITTEN\.\s+(.+)',
            # Fixed: Only extract program names in quotes to avoid keywords
            # CALL 'PROGRAM' or CALL "PROGRAM" - not CALL variable-name
            'call': r'CALL\s+[\'\"]([\w-]+)[\'\"]',
            # Fixed: Match READ/WRITE only at start of line (after whitespace)
            # This avoids matching "PERFORM 2100-READ-PAYMENT" or variable names
            # (?:^|\n) ensures READ/WRITE starts a new statement
            'file_read': r'(?:^|\n)\s+READ\s+([\w-]+)',
            'file_write': r'(?:^|\n)\s+WRITE\s+([\w-]+)',
            # Fixed: Extract internal file name (before ASSIGN), handle hyphens
            'file_select': r'SELECT\s+([\w-]+)\s+ASSIGN',
            'procedure': r'^\s{7,}([\w-]+)\.',
            'variable': r'^\s+\d+\s+([\w-]+)(?:\s+PIC|\s+OCCURS|\s+REDEFINES)',
            'copybook': r'COPY\s+([\w-]+)',
        }

        # System library and built-in calls to exclude from user program calls
        self.system_calls = {
            # IBM system calls
            'CBLTDLI', 'CEE3ABD', 'CEEDAYS', 'COBDATFT',
            'CEEGETAI', 'CEE3ADD', 'CEE3SUB', 'CEE3MUL', 'CEE3DIV',
            'CEETIMES', 'CEEFREXP', 'CEE3NEG', 'CEEABS', 'CEEMAX',
            'CEEMIN', 'CEEABS', 'CEEMOD', 'CEESQRT', 'CEEEXP',
            'CEELN', 'CEELOG10', 'CEESIN', 'CEECOS', 'CEETAN',
            # MQ API calls (IBM WebSphere MQ)
            'MQCLOSE', 'MQGET', 'MQOPEN', 'MQPUT', 'MQPUT1',
            'MQBACK', 'MQBEGIN', 'MQCMIT', 'MQDLH', 'MQGMO',
            'MQPMO', 'MQMD', 'MQXQH', 'MQAI',
            # NOTE: MVSWAIT and COBSWAIT are user programs in this environment
            # Only remove them if they are truly system libraries in your setup
            # CICS subsystem calls
            'CICS', 'EXEC', 'CALL-CICS',
            # Common keywords that might be extracted
            'IF', 'THEN', 'ELSE', 'TO', 'THRU', 'THROUGH',
            'FAIL', 'FAILED', 'ACCEPT', 'DISPLAY', 'STOP',
            'GO', 'PERFORM', 'CALL', 'EXIT', 'END',
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

            # Step 1: Strip COBOL line numbers (columns 1-6 in fixed format)
            content_no_line_nums = self._strip_cobol_line_numbers(content)

            # Step 2: Strip comments to avoid false matches
            code_only = self._strip_comments(content_no_line_nums)

            # Extract all entities
            parsed_data = {
                "program_name": self._extract_program_name(code_only),
                "author": self._extract_author(code_only),  # Use cleaned content for better matching
                "date_written": self._extract_date_written(code_only),  # Use cleaned content
                "calls": self._extract_calls(code_only),
                "files_read": self._extract_files_read(code_only, content_no_line_nums),
                "files_written": self._extract_files_written(code_only, content_no_line_nums),
                "files_declared": self._extract_files_declared(code_only),
                "procedures": self._extract_procedures(code_only),
                "variables": self._extract_variables(code_only),
                "copybooks": self._extract_copybooks(code_only),
                "loc": content.count('\n'),
                "complexity_score": self._calculate_complexity(code_only)
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

    def _strip_cobol_line_numbers(self, content: str) -> str:
        """
        Remove COBOL line numbers (columns 1-6) from fixed-format COBOL.

        COBOL fixed-format lines: [6-char line number][indicator][code]
        This function strips the first 6 characters from each line.

        Example:
            "000200 IDENTIFICATION DIVISION." -> "IDENTIFICATION DIVISION."
            "000210     PROGRAM-ID. MYPROGRAM." -> "    PROGRAM-ID. MYPROGRAM."
        """
        lines = content.split('\n')
        cleaned_lines = []

        for line in lines:
            # If line has at least 6 characters, strip them (they're typically line numbers)
            if len(line) >= 6:
                # Check if first 6 chars are all digits (typical COBOL line number format)
                if line[:6].isdigit() or (len(line) > 6 and line[:6].replace(' ', '').isdigit()):
                    # Keep the rest of the line starting from position 6
                    cleaned_lines.append(line[6:])
                else:
                    # Not a line number, keep original
                    cleaned_lines.append(line)
            else:
                # Line too short to have line number, keep as is
                cleaned_lines.append(line)

        return '\n'.join(cleaned_lines)

    def _strip_comments(self, content: str) -> str:
        """
        Remove COBOL comments to avoid false matches

        COBOL comments:
        - Lines starting with * in column 7
        - Inline comments after program lines are rare in standard COBOL
        """
        lines = content.split('\n')
        code_lines = []

        for line in lines:
            # Skip blank lines
            if not line.strip():
                code_lines.append('')
                continue

            # Check if line is a comment (column 7 = position 6 in 0-indexed)
            # Format: 6 chars (line number) + * + comment
            if len(line) > 6 and line[6] == '*':
                code_lines.append('')  # Preserve line numbers
                continue

            # Also handle lines that are just * (without line numbers)
            if line.lstrip().startswith('*'):
                code_lines.append('')
                continue

            code_lines.append(line)

        return '\n'.join(code_lines)

    def _extract_program_name(self, content: str) -> str:
        """
        Extract PROGRAM-ID, rejecting pure numeric IDs (line numbers).
        
        Valid program names are alphanumeric starting with a letter.
        Rejects pure digits which are likely malformed entries.
        """
        match = re.search(self.patterns['program_id'], content, re.IGNORECASE)
        if match:
            program_id = match.group(1).strip()
            
            # Reject pure numeric IDs (they're line numbers, not program names)
            if program_id and not program_id.isdigit():
                return program_id
            
            # If we got a pure number, it's a data error in the COBOL file
            # Try to find another PROGRAM-ID further in the file
            all_matches = re.findall(self.patterns['program_id'], content, re.IGNORECASE)
            for candidate in all_matches:
                if candidate.strip() and not candidate.strip().isdigit():
                    return candidate.strip()
        
        return "UNKNOWN"

    def _extract_author(self, content: str) -> str:
        """Extract AUTHOR"""
        match = re.search(self.patterns['author'], content, re.IGNORECASE)
        if match:
            author = match.group(1).strip()
            # Remove trailing period if present
            return author.rstrip('.')
        return ""

    def _extract_date_written(self, content: str) -> str:
        """Extract DATE-WRITTEN"""
        match = re.search(self.patterns['date_written'], content, re.IGNORECASE)
        return match.group(1).strip() if match else ""

    def _extract_calls(self, content: str) -> List[str]:
        """
        Extract CALL statements, filtering out system library calls.
        
        Only includes user-defined program calls, excluding:
        - IBM system calls (CEE*, CBLTDLI, etc.)
        - MQ API calls (MQOPEN, MQGET, MQPUT, etc.)
        - COBOL keywords
        - Wait routines
        """
        calls = re.findall(self.patterns['call'], content, re.IGNORECASE)
        
        # Filter out system calls (case-insensitive comparison)
        user_calls = [
            call for call in calls
            if call.upper() not in self.system_calls
        ]
        
        return list(set(user_calls))

    def _extract_files_read(self, content: str, original_content: str) -> List[str]:
        """
        Extract READ operations - maps record names to file names

        COBOL READ operates on FD names, not SELECT names.
        Example:
          SELECT ACCTFILE-FILE ...
          FD ACCTFILE-FILE.
          01 ACCTFILE-REC.
          ...
          READ ACCTFILE-FILE  <- This is what we capture
        """
        # Get FD names from FILE SECTION
        fd_to_select = self._build_fd_to_select_map(original_content)

        # Find all READ statements
        read_fds = re.findall(self.patterns['file_read'], content, re.IGNORECASE)

        # Map FD names to SELECT names (logical file names)
        file_names = []
        for fd_name in read_fds:
            # FD name usually matches SELECT name, or map via our dictionary
            select_name = fd_to_select.get(fd_name.upper(), fd_name)
            file_names.append(select_name)

        return list(set(file_names))

    def _extract_files_written(self, content: str, original_content: str) -> List[str]:
        """
        Extract WRITE operations - maps record names to file names

        COBOL WRITE operates on record names (01 level), not FD names.
        Example:
          SELECT OUT-FILE ...
          FD OUT-FILE.
          01 OUT-ACCT-REC.  <- Record name
          ...
          WRITE OUT-ACCT-REC  <- This is in the WRITE statement
        """
        # Build map of record names to FD names
        record_to_fd = self._build_record_to_fd_map(original_content)
        fd_to_select = self._build_fd_to_select_map(original_content)

        # Find all WRITE statements (they use record names)
        write_records = re.findall(self.patterns['file_write'], content, re.IGNORECASE)

        # Map record names -> FD names -> SELECT names
        # Only include records that actually map to FD declarations
        file_names = []
        for record_name in write_records:
            record_upper = record_name.upper()

            # Only process if this record is in our FILE SECTION map
            if record_upper in record_to_fd:
                fd_name = record_to_fd[record_upper]
                select_name = fd_to_select.get(fd_name, fd_name)
                file_names.append(select_name)
            # else: Skip - this is likely a WORKING-STORAGE variable, not a file record

        return list(set(file_names))

    def _extract_files_declared(self, content: str) -> List[str]:
        """Extract SELECT file declarations"""
        return list(set(re.findall(self.patterns['file_select'], content, re.IGNORECASE)))

    def _extract_procedures(self, content: str) -> List[str]:
        """
        Extract procedure/paragraph names.
        
        Filters out:
        - COBOL keywords and reserved words
        - Division names
        - Common system labels
        """
        procedures = re.findall(self.patterns['procedure'], content, re.MULTILINE | re.IGNORECASE)
        
        # Filter out DIVISION names and keywords
        filtered = [
            p for p in procedures 
            if 'DIVISION' not in p.upper() and p.upper() not in self.system_calls
        ]
        
        return list(set(filtered))[:50]

    def _extract_variables(self, content: str) -> List[str]:
        """Extract variable declarations"""
        variables = re.findall(self.patterns['variable'], content, re.MULTILINE | re.IGNORECASE)
        return list(set(variables))[:100]  # Limit to 100

    def _extract_copybooks(self, content: str) -> List[str]:
        """Extract COPY statements (copybook inclusions)"""
        return list(set(re.findall(self.patterns['copybook'], content, re.IGNORECASE)))

    def _build_fd_to_select_map(self, content: str) -> Dict[str, str]:
        """
        Build mapping of FD names to SELECT names

        Example:
          SELECT ACCTFILE-FILE ASSIGN TO ACCTFILE
          FD ACCTFILE-FILE.

        Returns: {'ACCTFILE-FILE': 'ACCTFILE-FILE'}
        """
        fd_map = {}

        # Extract all SELECT statements
        select_pattern = r'SELECT\s+([\w-]+)\s+ASSIGN\s+TO\s+([\w-]+)'
        select_matches = re.findall(select_pattern, content, re.IGNORECASE)

        for internal_name, external_name in select_matches:
            # Store as uppercase for case-insensitive lookup
            fd_map[internal_name.upper()] = internal_name

        return fd_map

    def _build_record_to_fd_map(self, content: str) -> Dict[str, str]:
        """
        Build mapping of record names (01 level) to FD names

        Example:
          FD OUT-FILE.
          01 OUT-ACCT-REC.

        Returns: {'OUT-ACCT-REC': 'OUT-FILE'}
        """
        record_map = {}

        # Find FILE SECTION
        file_section_match = re.search(
            r'FILE\s+SECTION\.(.*?)(?:WORKING-STORAGE\s+SECTION|LINKAGE\s+SECTION|PROCEDURE\s+DIVISION|$)',
            content,
            re.IGNORECASE | re.DOTALL
        )

        if not file_section_match:
            return record_map

        file_section = file_section_match.group(1)

        # Parse FD declarations and their 01 records
        # FD name followed by 01 record name (handles multi-line FD declarations)
        # Pattern explanation:
        # - FD followed by file name
        # - Optional period and any whitespace/content
        # - Match up to the 01 level record (using word boundary to match full "01")
        fd_pattern = r'FD\s+([\w-]+).*?^\s*01\s+([\w-]+)'
        fd_matches = re.findall(fd_pattern, file_section, re.IGNORECASE | re.DOTALL | re.MULTILINE)

        for fd_name, record_name in fd_matches:
            record_map[record_name.upper()] = fd_name.upper()

        return record_map

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
