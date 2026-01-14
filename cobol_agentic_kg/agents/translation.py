"""
Code Translation Agent - Translates COBOL programs to modern languages

Phase 1: LLM-based translation with Knowledge Graph context
- Supports: Java (Spring Boot), Python (FastAPI), C# (.NET Core)
- Uses rich business context from KG for intelligent translation
- Generates test stubs and conversion documentation
"""

import logging
import os
import re
from typing import Dict, List, Optional
from datetime import datetime
from pathlib import Path

from utils.neo4j_client import neo4j_client
from utils.llm_factory import get_llm
from utils.state import CodeTranslationState

logger = logging.getLogger(__name__)


# Language-specific configuration
LANGUAGE_CONFIG = {
    'java': {
        'extension': 'java',
        'test_extension': 'java',
        'frameworks': ['spring-boot', 'plain-java'],
        'default_framework': 'spring-boot'
    },
    'python': {
        'extension': 'py',
        'test_extension': 'py',
        'frameworks': ['fastapi', 'flask', 'plain-python'],
        'default_framework': 'fastapi'
    },
    'csharp': {
        'extension': 'cs',
        'test_extension': 'cs',
        'frameworks': ['dotnet-core', 'plain-csharp'],
        'default_framework': 'dotnet-core'
    }
}


class CodeTranslationAgent:
    """Agent for translating COBOL programs to modern languages using LLM"""

    def __init__(self):
        self.neo4j = neo4j_client
        self._llm = None
        self.exports_dir = Path("cobol_agentic_kg/exports/translations")

    @property
    def llm(self):
        """Lazy initialization of LLM"""
        if self._llm is None:
            # Use higher temperature for more creative, idiomatic code
            self._llm = get_llm(temperature=0.3, max_tokens=3000)
        return self._llm

    def process(self, state: CodeTranslationState) -> Dict:
        """
        Main processing method for code translation

        Args:
            state: CodeTranslationState with translation configuration

        Returns:
            Updated state with translations
        """
        logger.info(f"🔄 Starting code translation to {state['target_language']}...")
        start_time = datetime.now()

        try:
            state['status'] = 'translating'
            program_names = state['program_names']

            if not program_names:
                logger.warning("No programs specified for translation")
                state['status'] = 'completed'
                state['translations'] = []
                state['translation_time'] = 0.0
                return state

            logger.info(f"Translating {len(program_names)} program(s): {', '.join(program_names)}")

            # Translate each program
            translations = []
            for i, program_name in enumerate(program_names, 1):
                logger.info(f"  [{i}/{len(program_names)}] Translating {program_name}...")

                try:
                    translation = self._translate_program(program_name, state)
                    translations.append(translation)
                except Exception as e:
                    logger.error(f"Failed to translate {program_name}: {e}", exc_info=True)
                    translations.append({
                        'program_name': program_name,
                        'target_language': state['target_language'],
                        'translation_success': False,
                        'error_message': str(e)
                    })

            state['translations'] = translations
            state['status'] = 'completed'
            state['translation_time'] = (datetime.now() - start_time).total_seconds()
            state['timestamp'] = datetime.now().isoformat()

            success_count = sum(1 for t in translations if t['translation_success'])
            logger.info(f"✅ Translation complete: {success_count}/{len(program_names)} successful in {state['translation_time']:.2f}s")
            return state

        except Exception as e:
            logger.error(f"❌ Error in code translation: {e}", exc_info=True)
            state['status'] = 'failed'
            state['errors'].append(str(e))
            return state

    def _translate_program(self, program_name: str, state: CodeTranslationState) -> Dict:
        """
        Translate a single COBOL program

        Args:
            program_name: Name of program to translate
            state: Translation state with configuration

        Returns:
            Translation result dictionary
        """
        # 1. Get program context from KG
        program_data = self._get_program_context(program_name)

        if not program_data:
            raise ValueError(f"Program {program_name} not found in knowledge graph")

        # 2. Generate main code translation
        translation_result = self._generate_translation(
            program_data,
            state['target_language'],
            state['target_framework'],
            state['include_comments'],
            state.get('package_name')
        )

        # 3. Generate test stub if requested
        test_code = None
        test_file_path = None
        if state['include_tests']:
            test_code = self._generate_test_stub(
                program_data,
                translation_result,
                state['target_language'],
                state['target_framework']
            )

        # 4. Generate README with conversion notes
        readme_content = self._generate_readme(
            program_data,
            translation_result,
            state['target_language'],
            state['target_framework']
        )

        # 5. Save files to disk
        file_paths = self._save_translation_files(
            program_name,
            state['target_language'],
            translation_result['code'],
            test_code,
            readme_content
        )

        return {
            'program_name': program_name,
            'target_language': state['target_language'],
            'main_file_path': file_paths['main'],
            'test_file_path': file_paths.get('test'),
            'readme_path': file_paths['readme'],
            'main_code': translation_result['code'],
            'test_code': test_code,
            'conversion_notes': translation_result['notes'],
            'manual_review_items': translation_result['review_items'],
            'translation_success': True,
            'error_message': None
        }

    def _get_program_context(self, program_name: str) -> Optional[Dict]:
        """
        Query knowledge graph for comprehensive program data

        Args:
            program_name: Name of program

        Returns:
            Program data dictionary or None if not found
        """
        query = """
        MATCH (p:CobolProgram {name: $program_name})

        OPTIONAL MATCH (p)-[:CALLS]->(called:CobolProgram)
        OPTIONAL MATCH (caller:CobolProgram)-[:CALLS]->(p)
        OPTIONAL MATCH (p)-[:READS]->(reads:DataFile)
        OPTIONAL MATCH (p)-[:WRITES]->(writes:DataFile)

        WITH p,
             COLLECT(DISTINCT called.name) AS callees,
             COLLECT(DISTINCT caller.name) AS callers,
             COLLECT(DISTINCT reads.name) AS files_read,
             COLLECT(DISTINCT writes.name) AS files_written

        RETURN p.name AS name,
               COALESCE(p.domain, 'Unknown') AS domain,
               p.description AS description,
               COALESCE(p.business_logic, '') AS business_logic,
               COALESCE(p.complexity_score, 0) AS complexity,
               COALESCE(p.loc, 0) AS loc,
               p.code AS source_code,
               callees,
               callers,
               files_read,
               files_written
        """

        try:
            result = self.neo4j.query(query, {'program_name': program_name})
            if not result:
                logger.warning(f"Program {program_name} not found in knowledge graph")
                return None

            program_data = result[0]
            logger.info(f"Retrieved context for {program_name}: {program_data['loc']} LOC, domain: {program_data['domain']}")
            return program_data

        except Exception as e:
            logger.error(f"Error querying program context: {e}", exc_info=True)
            return None

    def _generate_translation(self, program_data: Dict, target_lang: str,
                            framework: str, include_comments: bool,
                            package_name: Optional[str]) -> Dict:
        """
        Generate code translation using LLM with rich context

        Args:
            program_data: Program data from KG
            target_lang: Target language
            framework: Target framework
            include_comments: Whether to preserve comments
            package_name: Optional package/namespace name

        Returns:
            Dictionary with code, notes, and review items
        """
        # Build comprehensive prompt
        prompt = self._build_translation_prompt(
            program_data,
            target_lang,
            framework,
            include_comments,
            package_name
        )

        try:
            logger.debug(f"Sending translation request to LLM for {program_data['name']}")
            response = self.llm.invoke(prompt)
            content = response.content

            # Parse LLM response
            code = self._extract_code_block(content, target_lang)
            notes = self._extract_notes(content)
            review_items = self._extract_review_items(content)

            return {
                'code': code,
                'notes': notes,
                'review_items': review_items
            }

        except Exception as e:
            logger.error(f"LLM translation failed: {e}", exc_info=True)
            raise

    def _build_translation_prompt(self, program_data: Dict, target_lang: str,
                                 framework: str, include_comments: bool,
                                 package_name: Optional[str]) -> str:
        """Build comprehensive translation prompt with KG context"""

        # Get framework-specific template
        framework_template = self._get_framework_template(target_lang, framework)

        # Build context section
        context = f"""
PROGRAM METADATA:
- Name: {program_data['name']}
- Domain: {program_data['domain']}
- Complexity: {program_data['complexity']}/100
- Lines of Code: {int(program_data['loc'])}
- Description: {program_data['description'] or 'Not available'}

BUSINESS LOGIC:
{program_data['business_logic'] or 'Not documented - please infer from code'}

DEPENDENCIES:
- Called by: {', '.join(program_data['callers']) if program_data['callers'] else 'None'}
- Calls: {', '.join(program_data['callees']) if program_data['callees'] else 'None'}

DATA OPERATIONS:
- Reads: {', '.join(program_data['files_read']) if program_data['files_read'] else 'None'}
- Writes: {', '.join(program_data['files_written']) if program_data['files_written'] else 'None'}

COBOL SOURCE CODE:
```cobol
{program_data['source_code'] or 'Source code not available'}
```
"""

        # Build main prompt
        lang_name = {'java': 'Java', 'python': 'Python', 'csharp': 'C#'}[target_lang]

        prompt = f"""You are an expert COBOL to {lang_name} translator with deep knowledge of both legacy and modern systems.

{context}

TASK: Translate this COBOL program to {lang_name} using {framework}.

{framework_template}

REQUIREMENTS:
1. **Preserve Business Logic**: Maintain exact same functionality and business rules
2. **Modern Patterns**: Use {lang_name} best practices and idiomatic code
3. **Data Structures**: Convert COBOL records/structures to {lang_name} classes/models
4. **File I/O**: Replace COBOL file handling with modern equivalents (database, ORM, etc.)
5. **Error Handling**: Add proper exception handling (COBOL lacks this)
6. **Comments**: {"Add detailed comments explaining COBOL origins and business logic" if include_comments else "Add minimal comments for complex logic only"}
{"7. **Package/Namespace**: Use '" + package_name + "' as the package/namespace" if package_name else ""}

OUTPUT FORMAT:
```{target_lang}
[Your complete translated code here - make it production-ready and well-structured]
```

CONVERSION NOTES:
- [List key changes made, e.g., "Converted COBOL PERFORM to private method"]
- [Explain any assumptions or interpretations]
- [Note any COBOL features that don't have direct equivalents]

MANUAL REVIEW REQUIRED:
- [List areas that need human verification, e.g., "Verify decimal precision in calculations"]
- [Flag any missing information or unclear business logic]
"""

        return prompt

    def _get_framework_template(self, target_lang: str, framework: str) -> str:
        """Get language/framework-specific guidance"""

        templates = {
            ('java', 'spring-boot'): """
SPRING BOOT CONVENTIONS:
- Use @Service annotation for business logic classes
- Use @Entity and @Table for data structures (map COBOL records)
- Use Spring Data JPA @Repository for file/database operations
- Use BigDecimal for COBOL PIC 9V99 (decimal) fields - CRITICAL for precision
- Use @Autowired for dependency injection
- Follow Java naming conventions (PascalCase for classes, camelCase for methods)
- Add JavaDoc comments with @param and @return
- Structure: One main service class with private helper methods for COBOL paragraphs
""",
            ('python', 'fastapi'): """
FASTAPI CONVENTIONS:
- Use Pydantic BaseModel for data structures (map COBOL records)
- Use SQLAlchemy for database operations (replace COBOL file I/O)
- Use Decimal type for COBOL decimal fields - CRITICAL for precision
- Follow PEP 8 style guide (snake_case for functions/variables)
- Add type hints throughout (from typing import List, Dict, etc.)
- Add docstrings with Args and Returns sections
- Structure: Main service class with helper methods, separate Pydantic models
""",
            ('csharp', 'dotnet-core'): """
.NET CORE CONVENTIONS:
- Use Entity Framework Core for data access
- Use classes with properties for data structures (map COBOL records)
- Use decimal type for COBOL decimal fields - CRITICAL for precision
- Follow C# naming conventions (PascalCase for classes/methods/properties)
- Add XML documentation comments (///)
- Use async/await patterns where appropriate
- Structure: Service class with domain models, dependency injection
""",
            ('java', 'plain-java'): """
PLAIN JAVA CONVENTIONS:
- Use POJOs for data structures with getters/setters
- Use BigDecimal for decimal fields
- Use JDBC or file I/O as appropriate
- Follow standard Java conventions
""",
            ('python', 'plain-python'): """
PLAIN PYTHON CONVENTIONS:
- Use dataclasses or regular classes for data structures
- Use Decimal for precision
- Follow PEP 8 style guide
- Add type hints and docstrings
""",
            ('csharp', 'plain-csharp'): """
PLAIN C# CONVENTIONS:
- Use classes with properties
- Use decimal type for precision
- Follow standard C# conventions
"""
        }

        return templates.get((target_lang, framework), "Use best practices for the language.")

    def _generate_test_stub(self, program_data: Dict, translation_result: Dict,
                          target_lang: str, framework: str) -> str:
        """Generate test stub using LLM"""

        prompt = f"""Generate a test stub for this translated COBOL program.

PROGRAM: {program_data['name']}
TRANSLATED CODE:
```{target_lang}
{translation_result['code']}
```

Generate a basic test structure with:
1. Test class/module setup
2. Test methods for main functionality (stubs with TODO comments)
3. Mock setup if needed (for dependencies, file I/O)
4. Example assertions (commented out)

Keep it simple - this is a STUB for developers to expand.
Use testing framework: {'JUnit 5' if target_lang == 'java' else 'pytest' if target_lang == 'python' else 'xUnit'}

Output only the test code, no explanations.
"""

        try:
            response = self.llm.invoke(prompt)
            return self._extract_code_block(response.content, target_lang)
        except Exception as e:
            logger.warning(f"Test stub generation failed: {e}")
            return self._generate_fallback_test_stub(program_data['name'], target_lang)

    def _generate_fallback_test_stub(self, program_name: str, target_lang: str) -> str:
        """Generate simple fallback test stub"""

        templates = {
            'java': f"""
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class {self._to_class_name(program_name)}Test {{

    @Test
    void testMainFunctionality() {{
        // TODO: Implement test
        fail("Test not implemented");
    }}
}}
""",
            'python': f"""
import pytest
from {self._to_snake_case(program_name)} import *

def test_main_functionality():
    # TODO: Implement test
    assert False, "Test not implemented"
""",
            'csharp': f"""
using Xunit;

public class {self._to_class_name(program_name)}Tests
{{
    [Fact]
    public void TestMainFunctionality()
    {{
        // TODO: Implement test
        Assert.True(false, "Test not implemented");
    }}
}}
"""
        }

        return templates.get(target_lang, "// Test stub not available")

    def _generate_readme(self, program_data: Dict, translation_result: Dict,
                        target_lang: str, framework: str) -> str:
        """Generate README with conversion notes"""

        lang_name = {'java': 'Java', 'python': 'Python', 'csharp': 'C#'}[target_lang]

        readme = f"""# {program_data['name']} - Translation Notes

## Original Program
- **Language**: COBOL
- **Domain**: {program_data['domain']}
- **Complexity**: {program_data['complexity']}/100
- **LOC**: {int(program_data['loc'])}
- **Description**: {program_data['description'] or 'Not available'}

## Translation Details
- **Target**: {lang_name} ({framework})
- **Generated**: {datetime.now().strftime('%Y-%m-%d %H:%M:%S UTC')}
- **Translation Method**: LLM-based with Knowledge Graph context

## Business Context
{program_data['business_logic'] or 'Business logic not documented in knowledge graph'}

## Conversion Notes
"""

        for i, note in enumerate(translation_result['notes'], 1):
            readme += f"{i}. {note}\n"

        if translation_result['review_items']:
            readme += "\n## Manual Review Required\n"
            for item in translation_result['review_items']:
                readme += f"- [ ] {item}\n"

        if program_data['callers'] or program_data['callees']:
            readme += "\n## Dependencies\n"
            if program_data['callers']:
                readme += f"- **Called by**: {', '.join(program_data['callers'])}\n"
            if program_data['callees']:
                readme += f"- **Calls**: {', '.join(program_data['callees'])}\n"

        if program_data['files_read'] or program_data['files_written']:
            readme += "\n## Data Operations\n"
            if program_data['files_read']:
                readme += f"- **Reads**: {', '.join(program_data['files_read'])}\n"
            if program_data['files_written']:
                readme += f"- **Writes**: {', '.join(program_data['files_written'])}\n"

        readme += f"""
## Next Steps
1. Review the generated code for correctness
2. Implement the test stubs
3. Test with real data
4. Integrate into your application
5. Address all manual review items above
"""

        return readme

    def _save_translation_files(self, program_name: str, target_lang: str,
                               main_code: str, test_code: Optional[str],
                               readme: str) -> Dict[str, str]:
        """Save translation files to disk"""

        # Create output directory
        output_dir = self.exports_dir / target_lang / program_name
        output_dir.mkdir(parents=True, exist_ok=True)

        config = LANGUAGE_CONFIG[target_lang]
        class_name = self._to_class_name(program_name)

        # Save main code file
        main_filename = f"{class_name}.{config['extension']}"
        main_path = output_dir / main_filename
        with open(main_path, 'w', encoding='utf-8') as f:
            f.write(main_code)

        paths = {'main': str(main_path)}

        # Save test file if provided
        if test_code:
            test_filename = f"{class_name}Test.{config['test_extension']}"
            if target_lang == 'python':
                test_filename = f"test_{self._to_snake_case(program_name)}.{config['test_extension']}"

            test_path = output_dir / test_filename
            with open(test_path, 'w', encoding='utf-8') as f:
                f.write(test_code)
            paths['test'] = str(test_path)

        # Save README
        readme_path = output_dir / 'README.md'
        with open(readme_path, 'w', encoding='utf-8') as f:
            f.write(readme)
        paths['readme'] = str(readme_path)

        logger.info(f"Saved translation files to {output_dir}")
        return paths

    # Helper methods for parsing and formatting

    def _extract_code_block(self, content: str, language: str) -> str:
        """Extract code block from LLM response"""
        # Try to find code fence with language
        pattern = rf"```{language}\n(.*?)```"
        match = re.search(pattern, content, re.DOTALL | re.IGNORECASE)

        if match:
            return match.group(1).strip()

        # Try generic code fence
        pattern = r"```\n(.*?)```"
        match = re.search(pattern, content, re.DOTALL)

        if match:
            return match.group(1).strip()

        # If no code fence, return everything (fallback)
        logger.warning("Could not find code fence in LLM response, returning full content")
        return content.strip()

    def _extract_notes(self, content: str) -> List[str]:
        """Extract conversion notes from LLM response"""
        notes = []

        # Look for CONVERSION NOTES section
        pattern = r"CONVERSION NOTES:?\n(.*?)(?:\n\n|MANUAL REVIEW|$)"
        match = re.search(pattern, content, re.DOTALL | re.IGNORECASE)

        if match:
            notes_text = match.group(1)
            # Extract bullet points or numbered items
            lines = notes_text.split('\n')
            for line in lines:
                line = line.strip()
                if line and (line.startswith('-') or line.startswith('*') or re.match(r'^\d+\.', line)):
                    # Remove leading markers
                    note = re.sub(r'^[-\*\d\.]+\s*', '', line)
                    if note:
                        notes.append(note)

        if not notes:
            notes.append("Standard COBOL to modern language translation")

        return notes

    def _extract_review_items(self, content: str) -> List[str]:
        """Extract manual review items from LLM response"""
        items = []

        # Look for MANUAL REVIEW section
        pattern = r"MANUAL REVIEW REQUIRED:?\n(.*?)(?:\n\n|$)"
        match = re.search(pattern, content, re.DOTALL | re.IGNORECASE)

        if match:
            review_text = match.group(1)
            lines = review_text.split('\n')
            for line in lines:
                line = line.strip()
                if line and (line.startswith('-') or line.startswith('*') or re.match(r'^\d+\.', line)):
                    item = re.sub(r'^[-\*\d\.]+\s*', '', line)
                    # Remove checkbox markers
                    item = re.sub(r'^\[\s*\]\s*', '', item)
                    if item:
                        items.append(item)

        return items

    def _to_class_name(self, program_name: str) -> str:
        """Convert COBOL program name to class name (PascalCase)"""
        # Remove common COBOL suffixes
        name = re.sub(r'(PROG|PGM|COBOL)$', '', program_name, flags=re.IGNORECASE)

        # Split on non-alphanumeric and capitalize each part
        parts = re.split(r'[^a-zA-Z0-9]+', name)
        class_name = ''.join(part.capitalize() for part in parts if part)

        return class_name or 'CobolProgram'

    def _to_snake_case(self, program_name: str) -> str:
        """Convert COBOL program name to snake_case"""
        # Remove common COBOL suffixes
        name = re.sub(r'(PROG|PGM|COBOL)$', '', program_name, flags=re.IGNORECASE)

        # Split on non-alphanumeric and join with underscore
        parts = re.split(r'[^a-zA-Z0-9]+', name)
        snake_name = '_'.join(part.lower() for part in parts if part)

        return snake_name or 'cobol_program'


# Global agent instance
code_translation_agent = CodeTranslationAgent()
