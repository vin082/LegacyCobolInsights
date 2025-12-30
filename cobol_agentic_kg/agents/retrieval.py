"""
Retrieval Agent - Executes queries and retrieves results
"""
from typing import List, Dict, Any
from langchain_openai import ChatOpenAI
from utils.state import CobolProcessingState
from utils.neo4j_client import neo4j_client
from utils.logger import logger
from config.settings import settings
from utils.cache import get_cache
import hashlib


class RetrievalAgent:
    """Agent responsible for executing queries and retrieving results"""

    def __init__(self):
        self.client = neo4j_client
        self._llm = None
        self._cache = None

    @property
    def cache(self):
        """Lazy initialization of cache client"""
        if self._cache is None:
            self._cache = get_cache()
        return self._cache

    @property
    def llm(self):
        """Lazy initialization of LLM for answer generation"""
        if self._llm is None:
            from config.settings import settings
            import os

            # Ensure API key is set
            api_key = settings.openai_api_key or os.environ.get("OPENAI_API_KEY")
            if not api_key:
                raise ValueError("OpenAI API key is not configured. Please set OPENAI_API_KEY in .env file")

            self._llm = ChatOpenAI(
                model="gpt-4o-mini",
                temperature=0,
                api_key=api_key,
                timeout=30,
                max_retries=2
            )
        return self._llm

    def process(self, state: CobolProcessingState) -> CobolProcessingState:
        """
        Execute Cypher query and retrieve results with caching

        Args:
            state: Current processing state

        Returns:
            Updated state with query results
        """
        cypher = state.get('generated_cypher')
        if not cypher:
            return state

        # Generate cache key from Cypher query
        cache_key = self._generate_cache_key(cypher)

        # Try to get from cache first
        cached_data = self.cache.get(cache_key)
        if cached_data and settings.cache_enabled:
            logger.info(f"🎯 RETRIEVAL AGENT: Cache HIT for query")
            return {
                **state,
                "query_results": cached_data.get("query_results", []),
                "answer": cached_data.get("answer", ""),
                "stage": "retrieval",
                "status": "completed",
                "cached": True
            }

        logger.info(f"🔎 RETRIEVAL AGENT: Executing Cypher query (cache miss)")

        try:
            # Execute query
            results = self.client.query(cypher)

            # Format results
            formatted_results = self._format_results(results)

            # Generate natural language answer
            user_query = state.get('user_query', '')
            answer = self._generate_answer(user_query, formatted_results, cypher)

            # Cache the results
            if settings.cache_enabled:
                self.cache.set(cache_key, {
                    "query_results": formatted_results,
                    "answer": answer
                }, ttl=settings.cache_ttl)

            return {
                **state,
                "query_results": formatted_results,
                "answer": answer,
                "stage": "retrieval",
                "status": "completed",
                "cached": False
            }

        except Exception as e:
            logger.error(f"Query execution failed: {e}")
            logger.error(f"Cypher: {cypher}")
            return {
                **state,
                "query_results": [],
                "answer": f"I encountered an error while processing your query: {str(e)}",
                "stage": "retrieval",
                "status": "failed",
                "errors": state.get('errors', []) + [f"Retrieval error: {str(e)}"],
                "cached": False
            }

    def _generate_cache_key(self, cypher: str) -> str:
        """
        Generate cache key from Cypher query

        Args:
            cypher: Cypher query string

        Returns:
            Cache key
        """
        # Normalize query (remove extra whitespace, convert to lowercase)
        normalized = ' '.join(cypher.lower().split())
        hash_value = hashlib.md5(normalized.encode()).hexdigest()
        return f"query:{hash_value}"

    def _format_results(self, results: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        Format query results for display

        Args:
            results: Raw query results

        Returns:
            Formatted results
        """
        if not results:
            return []

        # Limit results to prevent overwhelming the UI
        max_results = 100
        formatted = results[:max_results]

        # Add truncation notice if needed
        if len(results) > max_results:
            formatted.append({
                "_notice": f"Results truncated. Showing {max_results} of {len(results)} results."
            })

        return formatted

    def _generate_answer(self, user_query: str, results: List[Dict[str, Any]], cypher: str) -> str:
        """
        Generate natural language answer from query results

        Args:
            user_query: Original user question
            results: Formatted query results
            cypher: Executed Cypher query

        Returns:
            Natural language answer
        """
        if not results:
            return "No results found for your query. This could mean:\n- The data you're looking for doesn't exist in the knowledge graph\n- The query parameters didn't match any records\n- You may want to try rephrasing your question"

        # Filter out truncation notices
        actual_results = [r for r in results if "_notice" not in r]
        truncated = len(results) != len(actual_results)

        from langchain_core.prompts import ChatPromptTemplate

        prompt = ChatPromptTemplate.from_messages([
            ("system", """You are a COBOL legacy system expert helping technical managers understand their codebase.

Your role is to:
1. Provide clear, business-friendly explanations
2. Highlight impacts and dependencies
3. Use specific program names and details from the results
4. Explain implications for maintenance and changes
5. Be concise but informative (3-5 sentences)

Guidelines:
- Start with a direct answer to the question
- Mention specific program/file names from results
- Explain what this means for the business/maintenance
- If there are many results, summarize patterns
- Use plain English, avoid technical jargon when possible"""),
            ("user", """User Question: {user_query}

Query Results ({count} records):
{results}

Generate a clear, business-friendly answer.""")
        ])

        try:
            # Prepare results summary
            results_text = self._format_results_for_llm(actual_results[:20])  # Limit to first 20 for LLM

            logger.info(f"Generating natural language answer for {len(actual_results)} results")

            chain = prompt | self.llm
            response = chain.invoke({
                "user_query": user_query,
                "count": len(actual_results),
                "results": results_text
            })

            answer = response.content.strip()

            logger.info(f"Answer generated successfully: {len(answer)} characters")

            # Add truncation notice if applicable
            if truncated:
                answer += f"\n\n*Note: Results truncated. Showing first 100 of {len(results)} total results.*"

            return answer

        except Exception as e:
            logger.error(f"Answer generation failed: {e}", exc_info=True)
            # Return a basic summary instead of failing completely
            summary = self._generate_basic_summary(user_query, actual_results)
            return summary

    def _format_results_for_llm(self, results: List[Dict[str, Any]]) -> str:
        """
        Format results as text for LLM consumption

        Args:
            results: Query results

        Returns:
            Formatted string
        """
        if not results:
            return "No results"

        # Create a readable text representation
        lines = []
        for i, record in enumerate(results, 1):
            items = [f"{k}: {v}" for k, v in record.items()]
            lines.append(f"{i}. {', '.join(items)}")

        return "\n".join(lines)

    def _generate_basic_summary(self, user_query: str, results: List[Dict[str, Any]]) -> str:
        """
        Generate a basic summary without LLM when answer generation fails

        Args:
            user_query: Original user question
            results: Query results

        Returns:
            Basic text summary
        """
        if not results:
            return "No results found for your query."

        count = len(results)

        # Extract key information from first few results
        summary_lines = [f"Found {count} result{'s' if count != 1 else ''} for your query:"]

        # Show first 5 results
        for i, record in enumerate(results[:5], 1):
            items = [f"{k}: {v}" for k, v in record.items()]
            summary_lines.append(f"{i}. {', '.join(items)}")

        if count > 5:
            summary_lines.append(f"... and {count - 5} more results")

        summary_lines.append("\nPlease review the detailed results table below for complete information.")

        return "\n".join(summary_lines)


# Create singleton instance
retrieval_agent = RetrievalAgent()


# Wrapper function for LangGraph
def retrieval_agent_node(state: CobolProcessingState) -> CobolProcessingState:
    """LangGraph node wrapper"""
    return retrieval_agent.process(state)
