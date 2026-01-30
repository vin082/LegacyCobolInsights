"""
Custom evaluator functions for LangSmith evaluation
"""
import sys
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from utils.neo4j_client import neo4j_client
from utils.logger import logger


def cypher_validity_evaluator(run, example):
    """
    Check if generated Cypher query is syntactically valid

    Args:
        run: LangSmith run object with outputs
        example: Dataset example with expected outputs

    Returns:
        Dict with score and feedback
    """
    cypher = run.outputs.get("cypher", "")

    if not cypher:
        return {
            "key": "cypher_valid",
            "score": 0,
            "comment": "No Cypher query generated"
        }

    try:
        # Try to execute the query (with LIMIT 1 to be safe)
        if "LIMIT" not in cypher.upper():
            test_cypher = f"{cypher} LIMIT 1"
        else:
            test_cypher = cypher

        neo4j_client.query(test_cypher)

        return {
            "key": "cypher_valid",
            "score": 1,
            "comment": "Query is syntactically valid"
        }
    except Exception as e:
        return {
            "key": "cypher_valid",
            "score": 0,
            "comment": f"Syntax error: {str(e)}"
        }


def cypher_contains_keywords_evaluator(run, example):
    """
    Check if Cypher query contains expected keywords

    Args:
        run: LangSmith run object with outputs
        example: Dataset example with expected outputs containing "cypher_should_contain" list

    Returns:
        Dict with score and feedback
    """
    cypher = run.outputs.get("cypher", "").lower()
    expected_keywords = example.outputs.get("cypher_should_contain", [])

    if not expected_keywords:
        return {
            "key": "contains_keywords",
            "score": 1,
            "comment": "No keywords to check"
        }

    # Check which keywords are present
    found_keywords = [kw for kw in expected_keywords if kw.lower() in cypher]
    missing_keywords = [kw for kw in expected_keywords if kw.lower() not in cypher]

    score = len(found_keywords) / len(expected_keywords)

    comment = f"Found {len(found_keywords)}/{len(expected_keywords)} keywords"
    if missing_keywords:
        comment += f". Missing: {', '.join(missing_keywords)}"

    return {
        "key": "contains_keywords",
        "score": score,
        "comment": comment
    }


def cypher_avoids_keywords_evaluator(run, example):
    """
    Check if Cypher query avoids certain keywords (e.g., wrong node labels)

    Args:
        run: LangSmith run object with outputs
        example: Dataset example with expected outputs containing "cypher_should_not_contain" list

    Returns:
        Dict with score and feedback
    """
    cypher = run.outputs.get("cypher", "").lower()
    avoid_keywords = example.outputs.get("cypher_should_not_contain", [])

    if not avoid_keywords:
        return {
            "key": "avoids_keywords",
            "score": 1,
            "comment": "No keywords to avoid"
        }

    # Check which keywords to avoid are present (should be none)
    found_bad_keywords = [kw for kw in avoid_keywords if kw.lower() in cypher]

    score = 1.0 if not found_bad_keywords else 0.0

    comment = "Query correctly avoids bad keywords"
    if found_bad_keywords:
        comment = f"Query contains keywords it should avoid: {', '.join(found_bad_keywords)}"

    return {
        "key": "avoids_keywords",
        "score": score,
        "comment": comment
    }


def groundedness_evaluator(run, example):
    """
    Check if generated summary is grounded in the source data

    Args:
        run: LangSmith run object with outputs
        example: Dataset example with expected outputs

    Returns:
        Dict with score and feedback
    """
    summary = run.outputs.get("summary", "").lower()

    # Get expected groundedness indicators from example
    should_mention = example.outputs.get("summary_should_mention", [])
    should_avoid = example.outputs.get("summary_should_avoid", [])

    if not should_mention and not should_avoid:
        return {
            "key": "grounded",
            "score": 1,
            "comment": "No groundedness criteria specified"
        }

    # Check mentions (things that should be in summary)
    mentions_score = 1.0
    if should_mention:
        found_mentions = [term for term in should_mention if term.lower() in summary]
        mentions_score = len(found_mentions) / len(should_mention)

    # Check avoids (hallucinations - things that shouldn't be in summary)
    avoids_score = 1.0
    if should_avoid:
        found_hallucinations = [term for term in should_avoid if term.lower() in summary]
        avoids_score = 1.0 if not found_hallucinations else 0.0

    # Combined score
    final_score = (mentions_score + avoids_score) / 2

    comment = f"Mentions: {mentions_score:.0%}, Avoids hallucinations: {avoids_score:.0%}"

    return {
        "key": "grounded",
        "score": final_score,
        "comment": comment
    }


def result_count_evaluator(run, example):
    """
    Check if query returns expected number of results

    Args:
        run: LangSmith run object with outputs
        example: Dataset example with expected outputs

    Returns:
        Dict with score and feedback
    """
    results = run.outputs.get("results", [])
    expected_min = example.outputs.get("min_results", 0)
    expected_max = example.outputs.get("max_results", float('inf'))

    result_count = len(results)

    if expected_min <= result_count <= expected_max:
        return {
            "key": "result_count",
            "score": 1,
            "comment": f"Result count ({result_count}) within expected range [{expected_min}, {expected_max}]"
        }
    else:
        return {
            "key": "result_count",
            "score": 0,
            "comment": f"Result count ({result_count}) outside expected range [{expected_min}, {expected_max}]"
        }
