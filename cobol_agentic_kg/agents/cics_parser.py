"""
CICS Parser Agent - Extracts transaction and screen definitions

CICS (Customer Information Control System) is IBM's online transaction
processing system. This parser handles:

1. BMS (Basic Mapping Support) files (.bms):
   - Screen layouts and maps
   - Field definitions and positions
   - Screen navigation

2. CSD (CICS System Definition) files (.csd):
   - Transaction definitions
   - Program-to-transaction mappings
   - MAPSET definitions
   - Security and resource definitions

This enables:
- Understanding online vs batch programs
- Mapping business transactions to programs
- UI/screen flow analysis
- User interaction modeling
"""
import re
from typing import Dict, Any, List
from utils.state import CobolProcessingState
from utils.logger import logger


class CicsParser:
    """Agent responsible for parsing CICS-related files (BMS and CSD)"""

    def __init__(self):
        # BMS (Basic Mapping Support) patterns
        self.bms_patterns = {
            # MAPSET definition
            # Example: MAPNAME DFHMSD CTRL=(FREEKB),LANG=COBOL,MODE=INOUT
            'mapset': r'(\w+)\s+DFHMSD',

            # MAP definition (screen within mapset)
            # Example: MAPNAME DFHMDI SIZE=(24,80),COLUMN=1,LINE=1
            'map': r'(\w+)\s+DFHMDI',

            # Field definition
            # Example: FIELDNAME DFHMDF POS=(10,5),LENGTH=20,ATTRB=(NORM,PROT)
            'field': r'(\w+)\s+DFHMDF',

            # Position (row, col)
            'position': r'POS=\((\d+),(\d+)\)',

            # Length
            'length': r'LENGTH=(\d+)',

            # Initial value
            'initial': r'INITIAL=[\'"]([^\'"]+)[\'"]',

            # Attributes (ASKIP, PROT, NORM, BRT, DRK, etc.)
            'attributes': r'ATTRB=\(([^)]+)\)',

            # Color
            'color': r'COLOR=(\w+)',

            # Picture string (data type)
            'picin': r'PICIN=[\'"]([^\'"]+)[\'"]',
            'picout': r'PICOUT=[\'"]([^\'"]+)[\'"]',
        }

        # CSD (CICS System Definition) patterns
        self.csd_patterns = {
            # DEFINE PROGRAM statement
            # Example: DEFINE PROGRAM(MYPROG) GROUP(MYGROUP)
            'define_program': r'DEFINE\s+PROGRAM\((\w+)\)',

            # DEFINE TRANSACTION statement
            # Example: DEFINE TRANSACTION(MYTR) GROUP(MYGROUP)
            'define_transaction': r'DEFINE\s+TRANSACTION\((\w+)\)',

            # DEFINE MAPSET statement
            'define_mapset': r'DEFINE\s+MAPSET\((\w+)\)',

            # GROUP attribute
            'group': r'GROUP\((\w+)\)',

            # TRANSID attribute (transaction ID)
            'transid': r'TRANSID\((\w+)\)',

            # PROGRAM attribute (in transaction definition)
            'program': r'PROGRAM\((\w+)\)',

            # DESCRIPTION
            'description': r'DESCRIPTION\(([^)]+)\)',

            # LANGUAGE
            'language': r'LANGUAGE\((\w+)\)',

            # STATUS (ENABLED/DISABLED)
            'status': r'STATUS\((\w+)\)',
        }

    def process(self, state: CobolProcessingState) -> CobolProcessingState:
        """
        Parse CICS file (BMS or CSD) and extract definitions

        Args:
            state: Current processing state with file_content

        Returns:
            Updated state with cics_data containing appropriate structures
        """
        file_name = state['file_metadata']['filename']
        file_ext = state['file_metadata']['extension'].lower()

        logger.info(f"🖥️  CICS PARSER: Parsing {file_name} ({file_ext})")

        try:
            content = state['file_content']

            # Route to appropriate parser based on extension
            if file_ext in ['.bms']:
                cics_data = self._parse_bms(content, file_name)
                stage = "bms_parsing"
            elif file_ext in ['.csd']:
                cics_data = self._parse_csd(content, file_name)
                stage = "csd_parsing"
            else:
                raise ValueError(f"Unsupported CICS file type: {file_ext}")

            logger.info(f"✅ Parsed CICS file successfully")

            # Update state
            return {
                **state,
                "cics_data": cics_data,
                "stage": stage,
                "status": "completed"
            }

        except Exception as e:
            logger.error(f"❌ CICS parsing failed: {e}")
            return {
                **state,
                "cics_data": {},
                "stage": "cics_parsing",
                "status": "failed",
                "errors": state.get('errors', []) + [f"CICS parsing error: {str(e)}"]
            }

    def _parse_bms(self, content: str, file_name: str) -> Dict[str, Any]:
        """
        Parse BMS (Basic Mapping Support) file

        Example BMS:
            MYMAP  DFHMSD CTRL=(FREEKB),LANG=COBOL,MODE=INOUT
            MAP1   DFHMDI SIZE=(24,80),COLUMN=1,LINE=1
            FLD1   DFHMDF POS=(1,1),LENGTH=20,ATTRB=(NORM,PROT)
            FLD2   DFHMDF POS=(2,1),LENGTH=10,ATTRB=(UNPROT)

        Returns:
            {
                'type': 'BMS',
                'mapset_name': 'MYMAP',
                'maps': [
                    {
                        'map_name': 'MAP1',
                        'size': (24, 80),
                        'fields': [
                            {
                                'name': 'FLD1',
                                'position': (1, 1),
                                'length': 20,
                                'attributes': ['NORM', 'PROT']
                            },
                            ...
                        ]
                    }
                ]
            }
        """
        # Extract mapset name (first DFHMSD)
        mapset_match = re.search(self.bms_patterns['mapset'], content, re.MULTILINE | re.IGNORECASE)
        mapset_name = mapset_match.group(1) if mapset_match else file_name.replace('.bms', '').replace('.BMS', '')

        # Extract all maps
        maps = []
        lines = content.split('\n')

        current_map = None
        for line in lines:
            # Skip comments
            if line.strip().startswith('*'):
                continue

            # New map definition
            map_match = re.search(self.bms_patterns['map'], line, re.IGNORECASE)
            if map_match:
                # Save previous map if exists
                if current_map:
                    maps.append(current_map)

                # Start new map
                map_name = map_match.group(1)
                current_map = {
                    'map_name': map_name,
                    'fields': []
                }
                continue

            # Field definition
            if current_map:
                field_match = re.search(self.bms_patterns['field'], line, re.IGNORECASE)
                if field_match:
                    field_name = field_match.group(1)

                    # Extract position
                    pos_match = re.search(self.bms_patterns['position'], line)
                    position = (int(pos_match.group(1)), int(pos_match.group(2))) if pos_match else None

                    # Extract length
                    len_match = re.search(self.bms_patterns['length'], line)
                    length = int(len_match.group(1)) if len_match else None

                    # Extract attributes
                    attr_match = re.search(self.bms_patterns['attributes'], line)
                    attributes = attr_match.group(1).split(',') if attr_match else []

                    # Extract color
                    color_match = re.search(self.bms_patterns['color'], line)
                    color = color_match.group(1) if color_match else None

                    # Extract initial value
                    init_match = re.search(self.bms_patterns['initial'], line)
                    initial = init_match.group(1) if init_match else None

                    current_map['fields'].append({
                        'name': field_name,
                        'position': position,
                        'length': length,
                        'attributes': attributes,
                        'color': color,
                        'initial': initial
                    })

        # Add last map
        if current_map:
            maps.append(current_map)

        return {
            'type': 'BMS',
            'mapset_name': mapset_name,
            'maps': maps,
            'loc': content.count('\n')
        }

    def _parse_csd(self, content: str, file_name: str) -> Dict[str, Any]:
        """
        Parse CSD (CICS System Definition) file

        Example CSD:
            DEFINE PROGRAM(MYPROG) GROUP(MYGROUP)
                   LANGUAGE(COBOL) TRANSID(MYTR)
                   STATUS(ENABLED)
            DEFINE TRANSACTION(MYTR) GROUP(MYGROUP)
                   PROGRAM(MYPROG)
                   DESCRIPTION(My Transaction)

        Returns:
            {
                'type': 'CSD',
                'programs': [
                    {
                        'name': 'MYPROG',
                        'group': 'MYGROUP',
                        'language': 'COBOL',
                        'transid': 'MYTR',
                        'status': 'ENABLED'
                    }
                ],
                'transactions': [
                    {
                        'transid': 'MYTR',
                        'program': 'MYPROG',
                        'group': 'MYGROUP',
                        'description': 'My Transaction'
                    }
                ],
                'mapsets': [...]
            }
        """
        programs = []
        transactions = []
        mapsets = []

        # Split into definition blocks (each DEFINE is a block)
        lines = content.split('\n')

        current_def = None
        current_type = None
        current_lines = []

        for line in lines:
            # Check for new DEFINE statement
            prog_match = re.search(self.csd_patterns['define_program'], line, re.IGNORECASE)
            trans_match = re.search(self.csd_patterns['define_transaction'], line, re.IGNORECASE)
            map_match = re.search(self.csd_patterns['define_mapset'], line, re.IGNORECASE)

            if prog_match or trans_match or map_match:
                # Process previous definition
                if current_def and current_type:
                    self._process_csd_definition(current_def, current_type, current_lines, programs, transactions, mapsets)

                # Start new definition
                if prog_match:
                    current_def = prog_match.group(1)
                    current_type = 'PROGRAM'
                elif trans_match:
                    current_def = trans_match.group(1)
                    current_type = 'TRANSACTION'
                elif map_match:
                    current_def = map_match.group(1)
                    current_type = 'MAPSET'

                current_lines = [line]
            elif current_def:
                # Continuation of current definition
                current_lines.append(line)

        # Process last definition
        if current_def and current_type:
            self._process_csd_definition(current_def, current_type, current_lines, programs, transactions, mapsets)

        return {
            'type': 'CSD',
            'programs': programs,
            'transactions': transactions,
            'mapsets': mapsets,
            'loc': content.count('\n')
        }

    def _process_csd_definition(self, def_name: str, def_type: str, lines: List[str],
                                 programs: List, transactions: List, mapsets: List):
        """Helper to process a single CSD definition block"""
        # Merge all lines for easier regex matching
        full_text = ' '.join(lines)

        # Extract common attributes
        group_match = re.search(self.csd_patterns['group'], full_text, re.IGNORECASE)
        group = group_match.group(1) if group_match else None

        desc_match = re.search(self.csd_patterns['description'], full_text, re.IGNORECASE)
        description = desc_match.group(1) if desc_match else None

        status_match = re.search(self.csd_patterns['status'], full_text, re.IGNORECASE)
        status = status_match.group(1) if status_match else None

        if def_type == 'PROGRAM':
            lang_match = re.search(self.csd_patterns['language'], full_text, re.IGNORECASE)
            language = lang_match.group(1) if lang_match else None

            trans_match = re.search(self.csd_patterns['transid'], full_text, re.IGNORECASE)
            transid = trans_match.group(1) if trans_match else None

            programs.append({
                'name': def_name,
                'group': group,
                'language': language,
                'transid': transid,
                'status': status,
                'description': description
            })

        elif def_type == 'TRANSACTION':
            prog_match = re.search(self.csd_patterns['program'], full_text, re.IGNORECASE)
            program = prog_match.group(1) if prog_match else None

            transactions.append({
                'transid': def_name,
                'program': program,
                'group': group,
                'status': status,
                'description': description
            })

        elif def_type == 'MAPSET':
            mapsets.append({
                'name': def_name,
                'group': group,
                'status': status,
                'description': description
            })


# Create singleton instance
cics_parser = CicsParser()


# Wrapper function for LangGraph
def cics_parser_node(state: CobolProcessingState) -> CobolProcessingState:
    """LangGraph node wrapper"""
    return cics_parser.process(state)
