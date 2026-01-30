"""
JCL Parser Agent - Extracts job orchestration from JCL files

JCL (Job Control Language) defines batch job execution on mainframes.
It specifies:
- Job names and execution order
- Programs to execute (EXEC PGM=)
- File allocations (DD statements)
- Dataset mappings (logical → physical)
- Runtime parameters (PARM)
- Conditional execution (IF/THEN)
- Job dependencies

This parser extracts these to enable:
- Understanding batch processing workflows
- Mapping logical file names to physical datasets
- Identifying job dependencies
- Runtime parameter analysis
"""
import re
from typing import Dict, Any, List, Optional
from utils.state import CobolProcessingState
from utils.logger import logger


class JclParser:
    """Agent responsible for parsing JCL (Job Control Language) files"""

    def __init__(self):
        # Regex patterns for JCL constructs
        # Note: JCL has positional format - // in columns 1-2
        self.patterns = {
            # Job card - defines the job
            # Example: //JOBNAME JOB (ACCT),'DESCRIPTION',CLASS=A
            'job_card': r'^//(\w+)\s+JOB\s+',

            # Exec statement - executes a program or procedure
            # Examples:
            #   //STEP01  EXEC PGM=MYPROGRAM
            #   //STEP02  EXEC PROC=MYPROC
            'exec_pgm': r'^//(\w+)\s+EXEC\s+PGM=(\w+)',
            'exec_proc': r'^//(\w+)\s+EXEC\s+(?:PROC=)?(\w+)',

            # DD (Data Definition) statement - defines datasets
            # Example: //INFILE  DD DSN=PROD.DATA.FILE,DISP=SHR
            'dd_statement': r'^//(\w+)\s+DD\s+',

            # DSN (Dataset Name) parameter
            # Examples: DSN=PROD.ACCT.MASTER, DSN=&&TEMP
            'dsn': r'DSN=([^\s,]+)',

            # DISP (Disposition) parameter
            # Example: DISP=(NEW,CATLG,DELETE)
            'disp': r'DISP=\(([^)]+)\)',

            # PARM parameter - runtime parameters passed to program
            # Examples:
            #   PARM='DELETE=YES'
            #   PARM='DATE=20240101,MODE=PROD'
            'parm': r'PARM=[\'"]([^\'"]+)[\'"]',

            # COND (Condition) - conditional execution
            # Example: COND=(0,NE,STEP01)
            'cond': r'COND=\(([^)]+)\)',

            # IF/THEN/ELSE - conditional logic
            'if_statement': r'^//\s+IF\s+\(([^)]+)\)\s+THEN',
            'else_statement': r'^//\s+ELSE',
            'endif_statement': r'^//\s+ENDIF',

            # SET statement - define symbolic parameters
            # Example: //  SET ENVIRON='PROD'
            'set_statement': r'^//\s+SET\s+(\w+)=[\'"]?([^\'"]+)[\'"]?',

            # INCLUDE statement - include other JCL
            # Example: //  INCLUDE MEMBER=COMMONDD
            'include': r'^//\s+INCLUDE\s+MEMBER=(\w+)',

            # Comments
            'comment': r'^/\*',

            # Continuation - JCL line continues on next line
            'continuation': r',\s*$',
        }

    def process(self, state: CobolProcessingState) -> CobolProcessingState:
        """
        Parse JCL file and extract job definition

        Args:
            state: Current processing state with file_content

        Returns:
            Updated state with jcl_data containing:
            - job_name: Name of the job
            - steps: List of execution steps
            - datasets: List of dataset allocations
            - parameters: Runtime parameters
            - dependencies: Conditional logic
        """
        file_name = state['file_metadata']['filename']
        logger.info(f"📜 JCL PARSER: Parsing {file_name}")

        try:
            content = state['file_content']

            # Handle line continuations first
            content = self._handle_continuations(content)

            # Extract job name
            job_name = self._extract_job_name(content)

            # Extract execution steps
            steps = self._extract_steps(content)

            # Extract DD statements (file allocations)
            datasets = self._extract_datasets(content)

            # Extract SET statements (symbolic parameters)
            symbolic_params = self._extract_symbolic_params(content)

            # Extract conditional logic
            conditions = self._extract_conditions(content)

            # Extract INCLUDE statements
            includes = self._extract_includes(content)

            jcl_data = {
                "job_name": job_name,
                "steps": steps,  # List of execution steps with programs/procedures
                "datasets": datasets,  # List of DD allocations
                "symbolic_params": symbolic_params,  # SET variables
                "conditions": conditions,  # IF/THEN/ELSE logic
                "includes": includes,  # Included members
                "loc": content.count('\n'),
            }

            logger.info(f"✅ Extracted job '{job_name}' with {len(steps)} steps, {len(datasets)} datasets")

            # Update state
            return {
                **state,
                "jcl_data": jcl_data,
                "stage": "jcl_parsing",
                "status": "completed"
            }

        except Exception as e:
            logger.error(f"❌ JCL parsing failed: {e}")
            return {
                **state,
                "jcl_data": {},
                "stage": "jcl_parsing",
                "status": "failed",
                "errors": state.get('errors', []) + [f"JCL parsing error: {str(e)}"]
            }

    def _handle_continuations(self, content: str) -> str:
        """
        Handle JCL line continuations

        In JCL, a comma at end of line means continuation.
        Example:
            //STEP01  EXEC PGM=MYPROG,
            //             PARM='MODE=PROD'

        Joins into:
            //STEP01  EXEC PGM=MYPROG, PARM='MODE=PROD'
        """
        lines = content.split('\n')
        merged_lines = []
        current_line = ""

        for line in lines:
            # Skip comment lines
            if re.match(self.patterns['comment'], line):
                if current_line:
                    merged_lines.append(current_line)
                    current_line = ""
                merged_lines.append(line)
                continue

            # Check for continuation
            if re.search(self.patterns['continuation'], line):
                # Remove trailing comma and merge
                current_line += line.rstrip(',') + " "
            else:
                # End of statement
                current_line += line
                merged_lines.append(current_line)
                current_line = ""

        # Add any remaining line
        if current_line:
            merged_lines.append(current_line)

        return '\n'.join(merged_lines)

    def _extract_job_name(self, content: str) -> str:
        """
        Extract job name from JOB card

        Example:
            //MONTHLY JOB (ACCT123),'Monthly Report',CLASS=A

        Returns:
            'MONTHLY'
        """
        match = re.search(self.patterns['job_card'], content, re.MULTILINE)
        return match.group(1) if match else "UNKNOWN"

    def _extract_steps(self, content: str) -> List[Dict[str, Any]]:
        """
        Extract execution steps

        Example:
            //STEP01  EXEC PGM=CBACT01C,PARM='DELETE=YES'
            //STEP02  EXEC PROC=STDPROC

        Returns:
            [
                {
                    'step_name': 'STEP01',
                    'type': 'PGM',
                    'program': 'CBACT01C',
                    'parm': 'DELETE=YES',
                    'cond': None
                },
                {
                    'step_name': 'STEP02',
                    'type': 'PROC',
                    'procedure': 'STDPROC',
                    'parm': None,
                    'cond': None
                }
            ]
        """
        steps = []
        lines = content.split('\n')

        for line in lines:
            # Check for EXEC PGM=
            pgm_match = re.search(self.patterns['exec_pgm'], line)
            if pgm_match:
                step_name = pgm_match.group(1)
                program = pgm_match.group(2)

                # Extract PARM if present
                parm_match = re.search(self.patterns['parm'], line)
                parm = parm_match.group(1) if parm_match else None

                # Extract COND if present
                cond_match = re.search(self.patterns['cond'], line)
                cond = cond_match.group(1) if cond_match else None

                steps.append({
                    'step_name': step_name,
                    'type': 'PGM',
                    'program': program,
                    'parm': parm,
                    'cond': cond
                })
                continue

            # Check for EXEC PROC=
            proc_match = re.search(self.patterns['exec_proc'], line)
            if proc_match:
                step_name = proc_match.group(1)
                procedure = proc_match.group(2)

                # Extract PARM if present
                parm_match = re.search(self.patterns['parm'], line)
                parm = parm_match.group(1) if parm_match else None

                steps.append({
                    'step_name': step_name,
                    'type': 'PROC',
                    'procedure': procedure,
                    'parm': parm,
                    'cond': None
                })

        return steps

    def _extract_datasets(self, content: str) -> List[Dict[str, Any]]:
        """
        Extract DD (Data Definition) statements

        Example:
            //INFILE   DD DSN=PROD.ACCT.MASTER,DISP=SHR
            //OUTFILE  DD DSN=PROD.ACCT.EXTRACT,DISP=(NEW,CATLG,DELETE)
            //SYSOUT   DD SYSOUT=*

        Returns:
            [
                {
                    'ddname': 'INFILE',
                    'dsn': 'PROD.ACCT.MASTER',
                    'disp': 'SHR',
                    'type': 'DATASET'
                },
                {
                    'ddname': 'OUTFILE',
                    'dsn': 'PROD.ACCT.EXTRACT',
                    'disp': 'NEW,CATLG,DELETE',
                    'type': 'DATASET'
                },
                {
                    'ddname': 'SYSOUT',
                    'dsn': None,
                    'disp': None,
                    'type': 'SYSOUT'
                }
            ]
        """
        datasets = []
        lines = content.split('\n')

        for line in lines:
            # Check for DD statement
            dd_match = re.search(self.patterns['dd_statement'], line)
            if not dd_match:
                continue

            ddname = dd_match.group(1)

            # Extract DSN
            dsn_match = re.search(self.patterns['dsn'], line)
            dsn = dsn_match.group(1) if dsn_match else None

            # Extract DISP
            disp_match = re.search(self.patterns['disp'], line)
            disp = disp_match.group(1) if disp_match else None

            # Determine type
            if 'SYSOUT=' in line:
                ds_type = 'SYSOUT'
            elif 'DUMMY' in line:
                ds_type = 'DUMMY'
            elif dsn and dsn.startswith('&&'):
                ds_type = 'TEMPORARY'
            elif dsn:
                ds_type = 'DATASET'
            else:
                ds_type = 'UNKNOWN'

            datasets.append({
                'ddname': ddname,
                'dsn': dsn,
                'disp': disp,
                'type': ds_type
            })

        return datasets

    def _extract_symbolic_params(self, content: str) -> Dict[str, str]:
        """
        Extract SET statements (symbolic parameters)

        Example:
            //  SET ENVIRON='PROD'
            //  SET REGION='4M'

        Returns:
            {
                'ENVIRON': 'PROD',
                'REGION': '4M'
            }
        """
        params = {}
        lines = content.split('\n')

        for line in lines:
            match = re.search(self.patterns['set_statement'], line)
            if match:
                param_name = match.group(1)
                param_value = match.group(2)
                params[param_name] = param_value

        return params

    def _extract_conditions(self, content: str) -> List[Dict[str, Any]]:
        """
        Extract IF/THEN/ELSE conditional logic

        Example:
            //  IF (RC = 0) THEN
            //GOODSTEP EXEC PGM=REPORT
            //  ELSE
            //BADSTEP  EXEC PGM=ERROR
            //  ENDIF

        Returns:
            [
                {
                    'type': 'IF',
                    'condition': 'RC = 0',
                    'line': 10
                },
                {
                    'type': 'ELSE',
                    'line': 12
                },
                {
                    'type': 'ENDIF',
                    'line': 14
                }
            ]
        """
        conditions = []
        lines = content.split('\n')

        for i, line in enumerate(lines, 1):
            # IF statement
            if_match = re.search(self.patterns['if_statement'], line)
            if if_match:
                conditions.append({
                    'type': 'IF',
                    'condition': if_match.group(1),
                    'line': i
                })
                continue

            # ELSE statement
            if re.search(self.patterns['else_statement'], line):
                conditions.append({
                    'type': 'ELSE',
                    'line': i
                })
                continue

            # ENDIF statement
            if re.search(self.patterns['endif_statement'], line):
                conditions.append({
                    'type': 'ENDIF',
                    'line': i
                })

        return conditions

    def _extract_includes(self, content: str) -> List[str]:
        """
        Extract INCLUDE statements

        Example:
            //  INCLUDE MEMBER=COMMONDD

        Returns:
            ['COMMONDD']
        """
        includes = []
        lines = content.split('\n')

        for line in lines:
            match = re.search(self.patterns['include'], line)
            if match:
                includes.append(match.group(1))

        return includes


# Create singleton instance
jcl_parser = JclParser()


# Wrapper function for LangGraph
def jcl_parser_node(state: CobolProcessingState) -> CobolProcessingState:
    """LangGraph node wrapper"""
    return jcl_parser.process(state)
