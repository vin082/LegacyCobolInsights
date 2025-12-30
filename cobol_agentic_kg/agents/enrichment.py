"""
Enrichment Agent - Adds semantic understanding using LLM
"""
import json
from typing import Dict, Any
from langchain_openai import ChatOpenAI
from utils.state import CobolProcessingState
from utils.logger import logger
from config.settings import settings


class EnrichmentAgent:
    """Agent responsible for LLM-powered code enrichment"""

    def __init__(self):
        self._llm = None

    @property
    def llm(self):
        """Lazy initialization of LLM (only when needed)"""
        if self._llm is None:
            self._llm = ChatOpenAI(
                model=settings.llm_model,
                temperature=settings.llm_temperature,
                max_tokens=settings.llm_max_tokens
            )
        return self._llm

    def process(self, state: CobolProcessingState) -> CobolProcessingState:
        """
        Enrich parsed data with semantic understanding

        Args:
            state: Current processing state

        Returns:
            Updated state with enrichment data
        """
        program_name = state['parsed_data'].get('program_name', 'UNKNOWN')
        logger.info(f"🧠 ENRICHMENT AGENT: Enriching {program_name}")

        # Skip enrichment if disabled
        if not settings.enable_llm_enrichment:
            logger.info("LLM enrichment disabled, skipping...")
            return {
                **state,
                "enriched_data": self._create_default_enrichment(state),
                "stage": "enrichment",
                "status": "completed"
            }

        try:
            # Extract relevant information for LLM
            context = self._create_context(state)

            # Generate enrichment
            enriched_data = self._generate_enrichment(context, state)

            # Update state
            return {
                **state,
                "enriched_data": enriched_data,
                "stage": "enrichment",
                "status": "completed",
                "tokens_used": state.get('tokens_used', 0) + enriched_data.get('tokens_used', 0)
            }

        except Exception as e:
            logger.error(f"Enrichment failed: {e}")
            # Fall back to default enrichment
            return {
                **state,
                "enriched_data": self._create_default_enrichment(state),
                "stage": "enrichment",
                "status": "completed",
                "errors": state.get('errors', []) + [f"Enrichment warning: {str(e)} (using defaults)"]
            }

    def _create_context(self, state: CobolProcessingState) -> str:
        """Create context for LLM prompt"""
        parsed = state['parsed_data']
        content = state['file_content']

        # Extract comments
        comments = [line.strip()[1:].strip()
                   for line in content.split('\n')
                   if line.strip().startswith('*') and len(line.strip()) > 1]

        context = f"""Program: {parsed.get('program_name', 'UNKNOWN')}
Author: {parsed.get('author', 'Unknown')}
Lines of Code: {parsed.get('loc', 0)}
Complexity Score: {parsed.get('complexity_score', 0)}

Calls: {', '.join(parsed.get('calls', [])[:5])}
Files Read: {', '.join(parsed.get('files_read', [])[:5])}
Files Written: {', '.join(parsed.get('files_written', [])[:5])}
Procedures: {', '.join(parsed.get('procedures', [])[:10])}

Comments: {' '.join(comments[:5])}
"""
        return context

    def _generate_enrichment(self, context: str, state: CobolProcessingState) -> Dict[str, Any]:
        """Generate enrichment using LLM"""

        prompt = f"""Analyze this COBOL program and provide structured insights.

{context}

Provide a JSON response with:
1. "summary": One sentence describing what this program does
2. "business_domain": The business domain (CRM/Finance/HR/Batch/Reporting/Integration/Other)
3. "complexity_rating": Code complexity (Low/Medium/High)
4. "modernization_priority": Priority for modernization (Low/Medium/High/Critical)
5. "key_functions": List of 2-3 key functions this program performs
6. "technical_debt_indicators": List of 2-3 technical debt issues (if any)

Return ONLY valid JSON, no markdown or explanation:
{{
    "summary": "...",
    "business_domain": "...",
    "complexity_rating": "...",
    "modernization_priority": "...",
    "key_functions": ["...", "..."],
    "technical_debt_indicators": ["...", "..."]
}}
"""

        try:
            response = self.llm.invoke(prompt)
            content = response.content.strip()

            # Remove markdown code blocks if present
            if content.startswith('```'):
                content = content.split('```')[1]
                if content.startswith('json'):
                    content = content[4:]
                content = content.strip()

            enriched_data = json.loads(content)

            # Add token usage
            enriched_data['tokens_used'] = response.response_metadata.get('token_usage', {}).get('total_tokens', 0)

            return enriched_data

        except json.JSONDecodeError as e:
            logger.warning(f"Failed to parse LLM response as JSON: {e}")
            return self._create_default_enrichment(state)

    def _create_default_enrichment(self, state: CobolProcessingState) -> Dict[str, Any]:
        """Create default enrichment when LLM is unavailable"""
        parsed = state['parsed_data']

        return {
            "summary": f"COBOL program {parsed.get('program_name', 'UNKNOWN')}",
            "business_domain": "Unknown",
            "complexity_rating": self._infer_complexity(parsed),
            "modernization_priority": "Medium",
            "key_functions": self._infer_functions(parsed),
            "technical_debt_indicators": [],
            "tokens_used": 0
        }

    def _infer_complexity(self, parsed: Dict[str, Any]) -> str:
        """Infer complexity from parsed data"""
        score = parsed.get('complexity_score', 0)
        if score < 20:
            return "Low"
        elif score < 50:
            return "Medium"
        else:
            return "High"

    def _infer_functions(self, parsed: Dict[str, Any]) -> list:
        """Infer key functions from parsed data"""
        functions = []

        if parsed.get('files_read'):
            functions.append(f"Reads data from {', '.join(parsed['files_read'][:2])}")

        if parsed.get('files_written'):
            functions.append(f"Writes data to {', '.join(parsed['files_written'][:2])}")

        if parsed.get('calls'):
            functions.append(f"Calls subprograms: {', '.join(parsed['calls'][:2])}")

        return functions[:3]


# Create singleton instance
enrichment_agent = EnrichmentAgent()


# Wrapper function for LangGraph
def enrichment_agent_node(state: CobolProcessingState) -> CobolProcessingState:
    """LangGraph node wrapper"""
    return enrichment_agent.process(state)
