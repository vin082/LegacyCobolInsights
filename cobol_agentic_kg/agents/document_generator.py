"""
Document Generator Agent - Creates detailed technical documentation from knowledge graph
Focuses on program-level functional specifications for developers and business analysts
"""
from typing import Dict, Any, List
from datetime import datetime
from pathlib import Path
from utils.logger import logger
from utils.neo4j_client import neo4j_client
from utils.llm_factory import get_llm
from config.settings import settings
import re
from docx import Document
from docx.shared import Inches, Pt, RGBColor
from docx.enum.text import WD_PARAGRAPH_ALIGNMENT


class DocumentGeneratorAgent:
    """Agent responsible for generating detailed technical documentation for COBOL programs"""

    def __init__(self):
        self.neo4j = neo4j_client
        self.exports_dir = Path(__file__).parent.parent / "exports"
        self.templates_dir = Path(__file__).parent.parent / "templates"
        self.exports_dir.mkdir(exist_ok=True)
        self.llm = None

    def process(self, state: Dict[str, Any]) -> Dict[str, Any]:
        """
        Generate comprehensive technical documentation from KG data

        Args:
            state: Dict with doc_type, format, filters, program_name (optional)

        Returns:
            Updated state with file_path and status
        """
        doc_type = state.get('doc_type', 'system_overview')
        doc_format = state.get('format', 'markdown')
        filters = state.get('filters', {})
        program_name = state.get('program_name')  # For single program docs

        logger.info(f"📄 DOCUMENT GENERATOR: Creating {doc_type} in {doc_format}")

        try:
            # Initialize LLM
            self.llm = get_llm(temperature=0.3)

            # Step 1: Gather comprehensive data from Neo4j
            data = self._gather_comprehensive_data(doc_type, filters, program_name)

            # Step 2: Use LLM to generate professional documentation
            content = self._generate_technical_documentation(doc_type, data)

            # Step 3: Export to file (markdown or docx)
            if doc_format == 'markdown':
                file_path = self._export_file(doc_type, 'markdown', content, program_name)
            elif doc_format == 'docx':
                file_path = self._export_as_docx(doc_type, content, program_name)
            else:
                raise ValueError(f"Unsupported format: {doc_format}")

            logger.info(f"✅ Document generated: {file_path}")

            return {
                **state,
                "status": "completed",
                "file_path": str(file_path),
                "stage": "document_generation"
            }

        except Exception as e:
            logger.error(f"Document generation failed: {e}")
            return {
                **state,
                "status": "failed",
                "errors": state.get('errors', []) + [str(e)],
                "stage": "document_generation"
            }

    def _gather_comprehensive_data(self, doc_type: str, filters: Dict, program_name: str = None) -> Dict[str, Any]:
        """
        Query Neo4j for comprehensive program-level data
        """
        logger.info(f"📊 Gathering comprehensive data for {doc_type}")

        data = {}

        if doc_type == 'system_overview':
            # Get all programs for documentation
            data['programs'] = self._get_all_programs_for_documentation(filters)

            # Get system-level stats
            data['stats'] = self.neo4j.get_statistics()
            data['total_programs'] = len(data['programs'])

        elif doc_type == 'program_detail' and program_name:
            # Get detailed info for single program
            data['program_info'] = self._get_program_details(program_name)
            data['dependencies'] = self._get_program_dependencies(program_name)
            data['data_flows'] = self._get_program_data_flows(program_name)
            data['procedures'] = self._get_program_procedures(program_name)
            data['business_logic'] = self._extract_program_business_logic(program_name)
            data['copybooks'] = self._get_program_copybooks(program_name)
            data['jobs'] = self._get_program_jobs(program_name)

        data['generation_date'] = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        data['filters'] = filters

        return data

    def _get_all_programs_for_documentation(self, filters: Dict) -> List[Dict]:
        """Get all programs with comprehensive details for documentation"""

        # Build WHERE clause based on filters; domain uses a parameter to avoid injection
        where_clauses = []
        params: Dict[str, Any] = {}

        if filters.get('domain'):
            where_clauses.append("p.domain = $domain")
            params['domain'] = filters['domain']

        if filters.get('complexity'):
            if filters['complexity'] == 'low':
                where_clauses.append("p.complexity_score < 30")
            elif filters['complexity'] == 'medium':
                where_clauses.append("p.complexity_score >= 30 AND p.complexity_score < 70")
            elif filters['complexity'] == 'high':
                where_clauses.append("p.complexity_score >= 70")

        where_clause = "WHERE " + " AND ".join(where_clauses) if where_clauses else ""

        # Get limit from filters (default 10 for performance); coerce to int
        limit = int(filters.get('max_programs', 10))
        params['limit'] = limit

        query = f"""
        MATCH (p:CobolProgram)
        {where_clause}
        OPTIONAL MATCH (p)-[:CALLS]->(called:CobolProgram)
        OPTIONAL MATCH (caller:CobolProgram)-[:CALLS]->(p)
        OPTIONAL MATCH (p)-[:CONTAINS]->(proc:Procedure)
        OPTIONAL MATCH (p)-[r]->(f:DataFile)
        WHERE type(r) IN ['READS', 'WRITES']

        WITH p,
             COUNT(DISTINCT called) AS calls_out,
             COUNT(DISTINCT caller) AS calls_in,
             COUNT(DISTINCT proc) AS procedure_count,
             COUNT(DISTINCT CASE WHEN type(r) = 'READS' THEN f END) AS files_read,
             COUNT(DISTINCT CASE WHEN type(r) = 'WRITES' THEN f END) AS files_written,
             CASE WHEN p.code IS NOT NULL THEN size(split(p.code, '\\n')) ELSE 0 END AS estimated_loc

        RETURN p.name AS program_name,
               COALESCE(p.domain, 'Not Enriched') AS domain,
               p.description AS description,
               COALESCE(p.complexity_score, 0) AS complexity,
               COALESCE(p.loc, estimated_loc) AS loc,
               p.code AS source_code,
               calls_out,
               calls_in,
               procedure_count,
               files_read,
               files_written
        ORDER BY COALESCE(p.complexity_score, calls_in + calls_out) DESC
        LIMIT $limit
        """

        try:
            results = self.neo4j.query(query, params)
            logger.info(f"Retrieved {len(results)} programs for documentation")
            return results
        except Exception as e:
            logger.error(f"Error fetching programs: {e}")
            return []

    def _get_program_details(self, program_name: str) -> Dict[str, Any]:
        """Get detailed information for a specific program, including source code"""
        query = """
        MATCH (p:CobolProgram {name: $program_name})
        RETURN p.name AS name,
               p.domain AS domain,
               p.description AS description,
               p.complexity_score AS complexity_score,
               p.loc AS loc,
               p.file_path AS file_path,
               p.code AS source_code
        """
        try:
            result = self.neo4j.query(query, {'program_name': program_name})
            return result[0] if result else {}
        except Exception as e:
            logger.error(f"Error fetching details for {program_name}: {e}")
            return {}

    def _get_program_dependencies(self, program_name: str) -> Dict[str, Any]:
        """Get program dependencies (calls and called by)"""
        params = {'program_name': program_name}

        # Programs this program calls
        calls_query = """
        MATCH (p:CobolProgram {name: $program_name})-[:CALLS]->(called:CobolProgram)
        RETURN called.name AS program,
               called.domain AS domain,
               called.complexity_score AS complexity
        ORDER BY called.name
        """

        # Programs that call this program
        called_by_query = """
        MATCH (caller:CobolProgram)-[:CALLS]->(p:CobolProgram {name: $program_name})
        RETURN caller.name AS program,
               caller.domain AS domain,
               caller.complexity_score AS complexity
        ORDER BY caller.name
        """

        try:
            calls = self.neo4j.query(calls_query, params)
            called_by = self.neo4j.query(called_by_query, params)
            return {
                'calls': calls,
                'called_by': called_by
            }
        except Exception as e:
            logger.error(f"Error fetching dependencies for {program_name}: {e}")
            return {'calls': [], 'called_by': []}

    def _get_program_data_flows(self, program_name: str) -> Dict[str, Any]:
        """Get data file operations for a program"""
        query = """
        MATCH (p:CobolProgram {name: $program_name})-[r]->(f:DataFile)
        WHERE type(r) IN ['READS', 'WRITES']
        RETURN f.name AS file_name,
               type(r) AS operation,
               f.description AS file_description
        ORDER BY f.name, operation
        """

        try:
            results = self.neo4j.query(query, {'program_name': program_name})

            # Organize by file
            files = {}
            for row in results:
                file_name = row['file_name']
                if file_name not in files:
                    files[file_name] = {
                        'description': row.get('file_description', ''),
                        'operations': []
                    }
                files[file_name]['operations'].append(row['operation'])

            return files
        except Exception as e:
            logger.error(f"Error fetching data flows for {program_name}: {e}")
            return {}

    def _get_program_procedures(self, program_name: str) -> List[Dict]:
        """Get procedures/paragraphs within a program"""
        query = """
        MATCH (p:CobolProgram {name: $program_name})-[:CONTAINS]->(proc:Procedure)
        RETURN proc.name AS name,
               proc.type AS type,
               proc.description AS description
        ORDER BY proc.name
        LIMIT 100
        """

        try:
            return self.neo4j.query(query, {'program_name': program_name})
        except Exception as e:
            logger.error(f"Error fetching procedures for {program_name}: {e}")
            return []

    def _extract_program_business_logic(self, program_name: str) -> str:
        """Extract business logic description from enrichment data"""
        query = """
        MATCH (p:CobolProgram {name: $program_name})
        RETURN p.business_logic AS business_logic,
               p.description AS description
        """

        try:
            result = self.neo4j.query(query, {'program_name': program_name})
            if result:
                return result[0].get('business_logic') or result[0].get('description') or ''
            return ''
        except Exception as e:
            logger.error(f"Error fetching business logic for {program_name}: {e}")
            return ''

    def _get_program_copybooks(self, program_name: str) -> List[Dict]:
        """Get copybooks included by a program"""
        query = """
        MATCH (p:CobolProgram {name: $program_name})-[:INCLUDES]->(c:Copybook)
        RETURN c.name AS name
        ORDER BY c.name
        """
        try:
            return self.neo4j.query(query, {'program_name': program_name})
        except Exception as e:
            logger.error(f"Error fetching copybooks for {program_name}: {e}")
            return []

    def _get_program_jobs(self, program_name: str) -> List[Dict]:
        """Get JCL jobs that execute a program"""
        query = """
        MATCH (j:Job)-[:EXECUTES]->(p:CobolProgram {name: $program_name})
        RETURN j.name AS name,
               j.description AS description
        ORDER BY j.name
        """
        try:
            return self.neo4j.query(query, {'program_name': program_name})
        except Exception as e:
            logger.error(f"Error fetching jobs for {program_name}: {e}")
            return []

    def _generate_technical_documentation(self, doc_type: str, data: Dict) -> str:
        """Generate technical documentation using LLM"""

        if doc_type == 'system_overview':
            return self._generate_system_documentation(data)
        elif doc_type == 'program_detail':
            return self._generate_program_documentation(data)
        else:
            raise ValueError(f"Unknown doc_type: {doc_type}")

    def _generate_system_documentation(self, data: Dict) -> str:
        """Generate system-wide technical documentation with program details"""

        programs = data.get('programs', [])
        stats = data.get('stats', {})

        # Check enrichment status
        enriched_count = sum(1 for p in programs if p.get('domain') not in ['Not Enriched', None])
        not_enriched_count = len(programs) - enriched_count

        enrichment_warning = ""
        if not_enriched_count > 0:
            enrichment_warning = f"""
> ⚠️ **ENRICHMENT STATUS**: {not_enriched_count} out of {len(programs)} programs have not been enriched yet.
> These programs show "Domain: Not Enriched" and have limited metadata.
> The LLM will analyze source code directly to generate documentation for these programs.
>
> **Recommendation**: Run the enrichment agent on your COBOL files to get comprehensive metadata including:
> - Business domain classification
> - Complexity scoring
> - Business logic extraction
> - LOC counts
"""

        md = f"""# COBOL System Technical Documentation

**Document Type:** Technical Specification & Reference Guide
**Generated:** {data['generation_date']}
**Total Programs Documented:** {len(programs)}
**Enriched Programs:** {enriched_count} / {len(programs)}
**Purpose:** Detailed technical reference for developers and business analysts

{enrichment_warning}
---

## Table of Contents

1. [System Overview](#system-overview)
2. [Program Catalog](#program-catalog)
3. [Detailed Program Specifications](#detailed-program-specifications)

---

## 1. System Overview

This documentation provides comprehensive technical details for the COBOL system components. Each program is documented with:

- **Functional Purpose**: What the program does
- **Business Logic**: Key business rules and processes
- **Dependencies**: Programs it calls and programs that call it
- **Data Operations**: Files read and written
- **Internal Structure**: Procedures and paragraphs
- **Complexity Metrics**: Code complexity and size

### System Statistics

| Metric | Count |
|--------|-------|
| Total Programs | {stats['nodes'].get('CobolProgram', 0)} |
| Documented Programs | {len(programs)} |
| Total Data Files | {stats['nodes'].get('DataFile', 0)} |
| Total Procedures | {stats['nodes'].get('Procedure', 0)} |

---

## 2. Program Catalog

Quick reference table of all documented programs:

| Program Name | Domain | Complexity | LOC | Calls Out | Called By | Files Read | Files Written |
|--------------|--------|------------|-----|-----------|-----------|------------|---------------|
"""

        # Add program catalog
        for prog in programs:
            md += f"| {prog.get('program_name', 'N/A')} "
            md += f"| {prog.get('domain', 'N/A')} "
            md += f"| {prog.get('complexity') or 0} "
            md += f"| {prog.get('loc') or 0} "
            md += f"| {prog.get('calls_out') or 0} "
            md += f"| {prog.get('calls_in') or 0} "
            md += f"| {prog.get('files_read') or 0} "
            md += f"| {prog.get('files_written') or 0} |\n"

        md += "\n---\n\n## 3. Detailed Program Specifications\n\n"
        md += "The following sections provide detailed documentation for each program.\n\n"

        # Generate detailed docs for each program
        for idx, prog in enumerate(programs, 1):
            prog_name = prog.get('program_name', '')
            copybooks = self._get_program_copybooks(prog_name) if prog_name else []
            jobs = self._get_program_jobs(prog_name) if prog_name else []
            md += self._generate_single_program_doc(prog, idx, copybooks=copybooks, jobs=jobs)
            md += "\n---\n\n"

        md += f"""
**END OF DOCUMENT**

*Generated by COBOL Agentic Knowledge Graph - {data['generation_date']}*
*This documentation is automatically generated from the knowledge graph and enriched with LLM analysis.*
"""

        return md

    @staticmethod
    def _extract_paragraphs_from_source(source_code: str) -> List[Dict]:
        """
        Fallback: extract paragraph names from source code when the graph has no
        Procedure nodes.  Strategy: collect all PERFORM targets — these are the
        authoritative set of paragraph/section names actually invoked in the program.
        Filters out COBOL keywords that can follow PERFORM (e.g. UNTIL).
        """
        if not source_code:
            return []

        # Words that can legally follow PERFORM but are not paragraph names
        COBOL_KEYWORDS = {
            'UNTIL', 'VARYING', 'THRU', 'THROUGH', 'INLINE',
            'WITH', 'TEST', 'AFTER', 'BEFORE',
        }

        seen = set()
        paragraphs = []
        # Match PERFORM <name> — name is alphanumeric + hyphens
        for match in re.finditer(r'\bPERFORM\s+([\w-]+)', source_code, re.IGNORECASE):
            name = match.group(1).strip()
            upper = name.upper()
            if upper in COBOL_KEYWORDS or upper in seen:
                continue
            seen.add(upper)
            paragraphs.append({'name': name, 'type': 'Paragraph', 'description': None})

        return paragraphs

    def _generate_single_program_doc(self, prog_summary: Dict, index: int,
                                     copybooks: List[Dict] = None,
                                     jobs: List[Dict] = None) -> str:
        """Generate detailed documentation for a single program using LLM"""

        program_name = prog_summary.get('program_name', 'Unknown')

        # Get detailed data for this program
        prog_details = self._get_program_details(program_name)
        dependencies = self._get_program_dependencies(program_name)
        data_flows = self._get_program_data_flows(program_name)
        procedures = self._get_program_procedures(program_name)
        business_logic = self._extract_program_business_logic(program_name)

        # Source code: prefer prog_details (program_detail path), fall back to prog_summary (system_overview path)
        source_code = prog_details.get('source_code') or prog_summary.get('source_code') or ''
        code_snippet = source_code[:2000] if source_code else ''

        # If graph returned no procedures but we have source, extract paragraph names as fallback
        if not procedures and source_code:
            procedures = self._extract_paragraphs_from_source(source_code)
            logger.info(f"📝 Extracted {len(procedures)} paragraphs from source for {program_name}")

        # Check if enrichment has run
        is_enriched = prog_summary.get('domain') not in ['Not Enriched', None]

        # Build context based on what's available
        if is_enriched:
            context = f"""PROGRAM: {program_name}
DOMAIN: {prog_summary.get('domain', 'Unknown')}
DESCRIPTION: {prog_details.get('description', 'No description available')}
BUSINESS LOGIC: {business_logic or 'Not documented'}
COMPLEXITY: {prog_summary.get('complexity', 0)}
LINES OF CODE: {prog_summary.get('loc', 0)}"""
        else:
            context = f"""PROGRAM: {program_name}
DESCRIPTION: {prog_details.get('description', 'No description available')}
COMPLEXITY: {prog_summary.get('complexity', 0)}
LINES OF CODE: {prog_summary.get('loc', 0)}

NOTE: This program has not been enriched yet. Analyze the source code below to infer:
- Business domain
- Complexity level
- Business logic

SOURCE CODE SNIPPET (first 2000 characters):
```cobol
{code_snippet}
```"""

        # --- Rich data-file context for the LLM prompt ---
        data_files_context = ""
        if data_flows:
            data_files_context = "DATA FILES (from knowledge graph):\n"
            for fname, finfo in list(data_flows.items())[:10]:
                ops = ", ".join(set(finfo['operations']))
                desc = finfo.get('description') or 'No description'
                data_files_context += f"  - {fname}: operations=[{ops}], description={desc}\n"
        # If source is available, also note files referenced in source that may not be in the graph
        if source_code:
            source_upper = source_code.upper()
            select_files = re.findall(r'SELECT\s+([\w-]+)\s+ASSIGN', source_upper)
            graph_files_upper = {f.upper() for f in data_flows.keys()} if data_flows else set()
            extra_files = [f for f in select_files if f not in graph_files_upper]
            if extra_files:
                data_files_context += "\nAdditional files referenced in source (not tracked as graph edges):\n"
                for f in extra_files:
                    data_files_context += f"  - {f}\n"

        # --- Copybooks context ---
        copybooks_context = ""
        if copybooks:
            copybooks_context = f"COPYBOOKS INCLUDED ({len(copybooks)}):\n"
            copybooks_context += "  " + ", ".join(c['name'] for c in copybooks) + "\n"

        # --- JCL jobs context ---
        jobs_context = ""
        if jobs:
            jobs_context = f"JCL JOBS THAT EXECUTE THIS PROGRAM ({len(jobs)}):\n"
            for j in jobs:
                jobs_context += f"  - {j['name']}: {j.get('description') or 'No description'}\n"
        else:
            jobs_context = "JCL JOBS: None found in knowledge graph.\n"

        # --- Procedures context ---
        procedures_context = f"PROCEDURES / PARAGRAPHS ({len(procedures)}):\n"
        if procedures:
            procedures_context += "  " + ", ".join(p['name'] for p in procedures[:30]) + "\n"
            if len(procedures) > 30:
                procedures_context += f"  ...and {len(procedures) - 30} more\n"
        else:
            procedures_context += "  None found.\n"

        # --- Complexity flag for prompt ---
        complexity = prog_summary.get('complexity') or 0
        complexity_instruction = ""
        if complexity >= 70:
            complexity_instruction = f"\nIMPORTANT: This program has a HIGH complexity score of {complexity}. Flag this prominently in the Technical Notes section — explain likely complexity drivers and recommend refactoring or test coverage.\n"

        # Use LLM to generate comprehensive documentation
        prompt = f"""You are a technical writer creating detailed COBOL program documentation for developers and business analysts.

Generate comprehensive documentation for this COBOL program:

{context}

CALLS OUT TO ({len(dependencies.get('calls', []))} programs):
{[p['program'] for p in dependencies.get('calls', [])[:10]]}

CALLED BY ({len(dependencies.get('called_by', []))} programs):
{[p['program'] for p in dependencies.get('called_by', [])[:10]]}

{data_files_context}
{procedures_context}
{copybooks_context}
{jobs_context}
{complexity_instruction}
Generate documentation with these sections:

### 3.{index} {program_name}

#### Purpose and Overview
[2-3 sentences describing what this program does and its role in the system]

#### Business Logic
[Detailed explanation of the business rules and processes this program implements]

#### Key Functionality
[Bullet points of main functions/capabilities]

#### Data Operations
[Describe ALL data files it reads/writes and why — include both graph-tracked and source-referenced files. Use a table with columns: File, Organization/Access, Role]

#### Dependencies
[Explain programs it depends on and which depend on it. Note copybooks. Note any external runtime calls like CEE3ABD if present in source. Note JCL job linkage.]

#### Technical Notes
[Complexity concerns, performance considerations, maintenance notes. If complexity is high, flag prominently with specific drivers.]

Format as markdown. Be technical but clear. Focus on helping developers understand how to work with this program."""

        try:
            response = self.llm.invoke(prompt)
            llm_output = response.content.strip()

            # Append complexity warning block after LLM output if high complexity
            if complexity >= 70:
                llm_output += f"""

> **HIGH COMPLEXITY WARNING**: This program has a complexity score of {complexity}/100. Prioritize it for refactoring or comprehensive test coverage before any modifications."""

            return llm_output
        except Exception as e:
            logger.error(f"LLM documentation generation failed for {program_name}: {e}")

            # Fallback: Generate basic documentation without LLM
            return self._generate_basic_program_doc(program_name, prog_summary, prog_details,
                                                    dependencies, data_flows, procedures, index)

    def _generate_basic_program_doc(self, program_name: str, prog_summary: Dict,
                                    prog_details: Dict, dependencies: Dict,
                                    data_flows: Dict, procedures: List[Dict], index: int) -> str:
        """Generate basic documentation without LLM (fallback)"""

        md = f"""### 3.{index} {program_name}

#### Program Information

| Property | Value |
|----------|-------|
| **Program Name** | {program_name} |
| **Domain** | {prog_summary.get('domain', 'N/A')} |
| **Complexity Score** | {prog_summary.get('complexity') or 0} |
| **Lines of Code** | {prog_summary.get('loc') or 0} |
| **Description** | {prog_details.get('description', 'No description available')} |

#### Purpose and Overview

{prog_details.get('description', 'This program is part of the COBOL system.')}

"""

        # Dependencies
        calls = dependencies.get('calls', [])
        called_by = dependencies.get('called_by', [])

        if calls or called_by:
            md += "#### Dependencies\n\n"

            if calls:
                md += f"**Calls {len(calls)} programs:**\n\n"
                for dep in calls[:10]:
                    md += f"- `{dep['program']}` ({dep.get('domain', 'N/A')})\n"
                if len(calls) > 10:
                    md += f"\n*...and {len(calls) - 10} more*\n"
                md += "\n"

            if called_by:
                md += f"**Called by {len(called_by)} programs:**\n\n"
                for dep in called_by[:10]:
                    md += f"- `{dep['program']}` ({dep.get('domain', 'N/A')})\n"
                if len(called_by) > 10:
                    md += f"\n*...and {len(called_by) - 10} more*\n"
                md += "\n"

        # Data Operations
        if data_flows:
            md += "#### Data Operations\n\n"
            md += "| File Name | Operations | Description |\n"
            md += "|-----------|------------|-------------|\n"

            for file_name, file_info in data_flows.items():
                ops = ", ".join(set(file_info['operations']))
                desc = file_info.get('description', 'N/A')
                md += f"| {file_name} | {ops} | {desc} |\n"
            md += "\n"

        # Procedures
        if procedures:
            md += f"#### Internal Structure\n\n"
            md += f"This program contains {len(procedures)} procedures/paragraphs:\n\n"

            for proc in procedures[:20]:
                md += f"- **{proc['name']}**"
                if proc.get('description'):
                    md += f": {proc['description']}"
                md += "\n"

            if len(procedures) > 20:
                md += f"\n*...and {len(procedures) - 20} more procedures*\n"
            md += "\n"

        # Technical Notes
        complexity = prog_summary.get('complexity') or 0
        if complexity > 70:
            md += "#### Technical Notes\n\n"
            md += f"⚠️ **High Complexity Warning**: This program has a complexity score of {complexity}, "
            md += "which may make it difficult to maintain. Consider refactoring or adding comprehensive tests.\n\n"

        return md

    def _generate_program_documentation(self, data: Dict) -> str:
        """Generate documentation for a single program (detailed mode) with full skeleton"""

        prog_info = data.get('program_info', {})
        program_name = prog_info.get('name', 'Unknown')
        copybooks = data.get('copybooks', [])
        jobs = data.get('jobs', [])
        data_flows = data.get('data_flows', {})
        dependencies = data.get('dependencies', {})
        source_code = prog_info.get('source_code') or ''

        # Determine enrichment status
        is_enriched = prog_info.get('domain') not in ['Not Enriched', None]
        complexity = prog_info.get('complexity_score') or 0

        # Count source-referenced files via SELECT...ASSIGN
        source_file_count = len(re.findall(r'SELECT\s+[\w-]+\s+ASSIGN', source_code.upper())) if source_code else 0

        prog_summary = {
            'program_name': program_name,
            'domain': prog_info.get('domain'),
            'complexity': complexity,
            'loc': prog_info.get('loc'),
            'source_code': source_code
        }

        # --- Summary table ---
        enrichment_label = "Yes" if is_enriched else "No"
        enrichment_warning = ""
        if not is_enriched:
            enrichment_warning = f"""
> **Enrichment Note:** The knowledge graph does not contain enriched metadata (business_logic, description) for this program. The documentation below was generated by analyzing the COBOL source code directly.
"""

        complexity_label = f"{complexity} (HIGH)" if complexity >= 70 else f"{complexity} ({'MEDIUM' if complexity >= 30 else 'LOW'})"

        md = f"""# COBOL Program Technical Documentation — {program_name}

**Document Type:** Program Detail Specification
**Generated:** {data['generation_date']}
**Program:** {program_name}
**Domain:** {prog_info.get('domain') or 'Not Enriched'}
**Enrichment Status:** {'Enriched' if is_enriched else 'Not Enriched — documentation inferred from source code + knowledge graph'}

---

## Program Summary

| Property | Value |
|----------|-------|
| Program Name | {program_name} |
| Domain | {prog_info.get('domain') or 'Not Enriched'} |
| Complexity Score | {complexity_label} |
| Lines of Code | {prog_info.get('loc') or 0} |
| Source Code Available | {'Yes' if source_code else 'No'} |
| Enriched | {enrichment_label} |
| Outbound Calls | {len(dependencies.get('calls', []))} |
| Inbound Calls | {len(dependencies.get('called_by', []))} |
| Data Files (graph) | {len(data_flows)} |
| Data Files (source) | {source_file_count} |
| Copybooks | {len(copybooks)} |
| JCL Jobs | {len(jobs)} |

{enrichment_warning}
---

"""

        # --- LLM-generated body ---
        md += self._generate_single_program_doc(prog_summary, 1, copybooks=copybooks, jobs=jobs)

        # --- Footer ---
        md += f"""

---

**END OF DOCUMENT**

*Generated by COBOL Agentic Knowledge Graph*
*Source: Neo4j Knowledge Graph + COBOL source code analysis*
*Generated: {data['generation_date']}*
"""

        return md

    def _export_file(self, doc_type: str, doc_format: str, content: str, program_name: str = None) -> Path:
        """Export document to file"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

        if program_name:
            filename = f"TechDoc_{program_name}_{timestamp}.{self._get_file_extension(doc_format)}"
        else:
            filename = f"TechDoc_{doc_type}_{timestamp}.{self._get_file_extension(doc_format)}"

        file_path = self.exports_dir / filename
        file_path.write_text(content, encoding='utf-8')

        logger.info(f"💾 Exported technical documentation to: {file_path}")
        return file_path

    def _get_file_extension(self, doc_format: str) -> str:
        """Get file extension for format"""
        extensions = {
            'markdown': 'md',
            'docx': 'docx',
            'word': 'docx',
            'pdf': 'pdf'
        }
        return extensions.get(doc_format, 'txt')

    def _export_as_docx(self, doc_type: str, markdown_content: str, program_name: str = None) -> Path:
        """
        Convert markdown documentation to DOCX format

        Args:
            doc_type: Type of document
            markdown_content: Markdown formatted content
            program_name: Optional program name for filename

        Returns:
            Path to exported DOCX file
        """
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

        if program_name:
            filename = f"TechDoc_{program_name}_{timestamp}.docx"
        else:
            filename = f"TechDoc_{doc_type}_{timestamp}.docx"

        file_path = self.exports_dir / filename

        # Create DOCX document
        doc = Document()

        # Process markdown line by line
        lines = markdown_content.split('\n')
        i = 0

        while i < len(lines):
            line = lines[i].strip()

            if not line:
                i += 1
                continue

            # Main title (# Title)
            if line.startswith('# ') and not line.startswith('##'):
                title_text = line.replace('# ', '')
                title = doc.add_heading(title_text, level=0)
                title.alignment = WD_PARAGRAPH_ALIGNMENT.CENTER

            # Heading level 2 (## Heading)
            elif line.startswith('## '):
                heading_text = re.sub(r'^##\s+', '', line)
                heading_text = re.sub(r'[📋🏗️🔄🔗💼💥⚠️🎯📎]', '', heading_text).strip()
                doc.add_heading(heading_text, level=1)

            # Heading level 3 (### Heading)
            elif line.startswith('### '):
                heading_text = re.sub(r'^###\s+', '', line)
                heading_text = re.sub(r'^\d+\.\d+\s+', '', heading_text)  # Remove numbering like "3.1"
                doc.add_heading(heading_text, level=2)

            # Heading level 4 (#### Heading)
            elif line.startswith('#### '):
                heading_text = re.sub(r'^####\s+', '', line)
                doc.add_heading(heading_text, level=3)

            # Table detection
            elif '|' in line and i + 1 < len(lines) and '|' in lines[i + 1]:
                table_lines = [line]
                i += 1
                # Collect all table lines
                while i < len(lines) and '|' in lines[i]:
                    table_lines.append(lines[i].strip())
                    i += 1
                i -= 1

                # Parse and create table
                self._add_table_to_doc(doc, table_lines)

            # Blockquote / Warning (> text)
            elif line.startswith('>'):
                quote_lines = []
                while i < len(lines) and lines[i].strip().startswith('>'):
                    quote_lines.append(lines[i].strip().replace('> ', '').replace('>', ''))
                    i += 1
                i -= 1

                para = doc.add_paragraph('\n'.join(quote_lines))
                para.paragraph_format.left_indent = Inches(0.5)
                para.runs[0].font.italic = True
                para.runs[0].font.color.rgb = RGBColor(128, 128, 128)

            # Bullet list (- item or * item)
            elif line.startswith('- ') or line.startswith('* '):
                list_text = re.sub(r'^[-\*]\s+', '', line)
                list_text = re.sub(r'\*\*(.*?)\*\*', r'\1', list_text)  # Remove bold markers
                list_text = re.sub(r'`(.*?)`', r'\1', list_text)  # Remove code markers
                doc.add_paragraph(list_text, style='List Bullet')

            # Horizontal rule (---)
            elif line.startswith('---'):
                doc.add_paragraph()  # Just add spacing

            # Code block (```)
            elif line.startswith('```'):
                i += 1
                code_lines = []
                while i < len(lines) and not lines[i].strip().startswith('```'):
                    code_lines.append(lines[i])
                    i += 1
                if code_lines:
                    para = doc.add_paragraph('\n'.join(code_lines))
                    para.runs[0].font.name = 'Courier New'
                    para.runs[0].font.size = Pt(9)
                    para.paragraph_format.left_indent = Inches(0.5)

            # Bold text (**text** or __text__)
            elif '**' in line or '__' in line:
                para = doc.add_paragraph()
                self._add_formatted_text(para, line)

            # Regular paragraph
            else:
                # Clean up markdown formatting
                text = re.sub(r'\*\*(.*?)\*\*', r'\1', line)  # Remove bold
                text = re.sub(r'`(.*?)`', r'\1', text)  # Remove code
                text = re.sub(r'\[(.*?)\]\(.*?\)', r'\1', text)  # Remove links
                if text.strip():
                    doc.add_paragraph(text)

            i += 1

        # Save document
        doc.save(str(file_path))
        logger.info(f"💾 Exported DOCX documentation to: {file_path}")

        return file_path

    def _add_table_to_doc(self, doc, table_lines: List[str]):
        """Add a markdown table to the DOCX document"""
        if len(table_lines) < 2:
            return

        # Parse table
        rows = []
        for line in table_lines:
            if '---' in line and all(c in '|-: ' for c in line):
                continue  # Skip separator line
            cells = [cell.strip() for cell in line.split('|') if cell.strip()]
            if cells:
                rows.append(cells)

        if not rows:
            return

        # Create table
        table = doc.add_table(rows=len(rows), cols=len(rows[0]))
        table.style = 'Light Grid Accent 1'

        # Fill table
        for i, row_data in enumerate(rows):
            for j, cell_data in enumerate(row_data):
                cell = table.rows[i].cells[j]
                # Clean markdown formatting
                text = re.sub(r'\*\*(.*?)\*\*', r'\1', cell_data)
                text = re.sub(r'`(.*?)`', r'\1', text)
                text = re.sub(r'[🔴🟡🟢⚠️✅❌📊💰💵💳]', '', text).strip()
                cell.text = text

                # Bold header row
                if i == 0:
                    cell.paragraphs[0].runs[0].font.bold = True

        doc.add_paragraph()  # Add spacing after table

    def _add_formatted_text(self, paragraph, text: str):
        """Add text with markdown formatting to a paragraph"""
        # Simple bold detection
        parts = re.split(r'(\*\*.*?\*\*)', text)
        for part in parts:
            if part.startswith('**') and part.endswith('**'):
                run = paragraph.add_run(part.strip('*'))
                run.font.bold = True
            else:
                paragraph.add_run(part)


# Create singleton instance
document_generator_agent = DocumentGeneratorAgent()


# Wrapper function for LangGraph (if needed)
def document_generator_agent_node(state: Dict[str, Any]) -> Dict[str, Any]:
    """LangGraph node wrapper"""
    return document_generator_agent.process(state)
