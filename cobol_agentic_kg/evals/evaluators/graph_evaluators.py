"""
Graph Builder Evaluator Functions

These evaluators validate that the Graph Builder Agent:
1. Creates correct nodes with proper labels and properties
2. Creates correct relationships with proper types and directions
3. Implements MERGE logic correctly (no duplicates)
4. Maintains graph consistency
5. Handles edge cases (cyclic dependencies, re-ingestion, etc.)
"""
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from utils.neo4j_client import neo4j_client
from typing import Dict, List, Any


def node_existence_evaluator(run, example):
    """
    Check if all expected nodes were created in the graph

    Args:
        run: LangSmith run object with outputs
        example: Dataset example with expected outputs

    Returns:
        Dict with score (0-1) and feedback
    """
    expected_nodes = example.outputs.get("expected_nodes", [])

    if not expected_nodes:
        return {
            "key": "node_existence",
            "score": 1,
            "comment": "No nodes specified for validation"
        }

    found_count = 0
    missing_nodes = []

    for expected_node in expected_nodes:
        label = expected_node["label"]
        name = expected_node["properties"]["name"]

        # Query to check if node exists
        query = f"""
        MATCH (n:{label} {{name: $name}})
        RETURN count(n) as count
        """

        try:
            result = neo4j_client.query(query, {"name": name})

            if result and result[0]['count'] > 0:
                found_count += 1
            else:
                missing_nodes.append(f"{label}:{name}")
        except Exception as e:
            missing_nodes.append(f"{label}:{name} (query error)")

    score = found_count / len(expected_nodes)

    comment = f"Found {found_count}/{len(expected_nodes)} expected nodes"
    if missing_nodes:
        comment += f". Missing: {', '.join(missing_nodes[:3])}"
        if len(missing_nodes) > 3:
            comment += f" and {len(missing_nodes) - 3} more"

    return {
        "key": "node_existence",
        "score": score,
        "comment": comment
    }


def node_properties_evaluator(run, example):
    """
    Check if node properties match expected values

    Args:
        run: LangSmith run object with outputs
        example: Dataset example with expected outputs

    Returns:
        Dict with score (0-1) and feedback
    """
    expected_nodes = example.outputs.get("expected_nodes", [])

    total_props = 0
    correct_props = 0
    property_errors = []

    for expected_node in expected_nodes:
        label = expected_node["label"]
        name = expected_node["properties"]["name"]
        expected_props = expected_node["properties"]

        # Query to get node properties
        query = f"""
        MATCH (n:{label} {{name: $name}})
        RETURN n
        """

        try:
            result = neo4j_client.query(query, {"name": name})

            if not result:
                continue

            actual_node = result[0]['n']

            # Check each expected property
            for prop_name, expected_value in expected_props.items():
                total_props += 1
                actual_value = actual_node.get(prop_name)

                # Handle None/null comparisons
                if expected_value is None and actual_value is None:
                    correct_props += 1
                elif actual_value == expected_value:
                    correct_props += 1
                else:
                    property_errors.append(
                        f"{name}.{prop_name}: expected {expected_value}, got {actual_value}"
                    )
        except Exception as e:
            property_errors.append(f"{name}: query error - {str(e)}")

    if total_props == 0:
        return {
            "key": "node_properties",
            "score": 1,
            "comment": "No properties to validate"
        }

    score = correct_props / total_props
    comment = f"Correct properties: {correct_props}/{total_props}"

    if property_errors:
        comment += f". Errors: {property_errors[0]}"
        if len(property_errors) > 1:
            comment += f" and {len(property_errors) - 1} more"

    return {
        "key": "node_properties",
        "score": score,
        "comment": comment
    }


def relationship_existence_evaluator(run, example):
    """
    Check if all expected relationships exist with correct types and directions

    Args:
        run: LangSmith run object with outputs
        example: Dataset example with expected outputs

    Returns:
        Dict with score (0-1) and feedback
    """
    expected_rels = example.outputs.get("expected_relationships", [])

    if not expected_rels:
        return {
            "key": "relationship_existence",
            "score": 1,
            "comment": "No relationships to validate"
        }

    found_count = 0
    missing_rels = []

    for rel in expected_rels:
        rel_type = rel["type"]
        from_name = rel.get("from") or rel.get("from_node", {}).get("name")
        to_name = rel.get("to") or rel.get("to_node", {}).get("name")

        if not from_name or not to_name:
            missing_rels.append(f"Invalid relationship spec: {rel}")
            continue

        # Query to check relationship existence
        query = f"""
        MATCH (from {{name: $from_name}})-[r:{rel_type}]->(to {{name: $to_name}})
        RETURN count(r) as count
        """

        try:
            result = neo4j_client.query(query, {
                "from_name": from_name,
                "to_name": to_name
            })

            if result and result[0]['count'] > 0:
                found_count += 1
            else:
                missing_rels.append(f"{from_name}-[{rel_type}]->{to_name}")
        except Exception as e:
            missing_rels.append(f"{from_name}-[{rel_type}]->{to_name} (error)")

    score = found_count / len(expected_rels)
    comment = f"Found {found_count}/{len(expected_rels)} expected relationships"

    if missing_rels:
        comment += f". Missing: {', '.join(missing_rels[:3])}"
        if len(missing_rels) > 3:
            comment += f" and {len(missing_rels) - 3} more"

    return {
        "key": "relationship_existence",
        "score": score,
        "comment": comment
    }


def validation_query_evaluator(run, example):
    """
    Run custom validation queries and compare results

    Most flexible evaluator - can test any graph pattern

    Args:
        run: LangSmith run object with outputs
        example: Dataset example with expected outputs

    Returns:
        Dict with score (0-1) and feedback
    """
    validation_queries = example.outputs.get("validation_queries", [])

    if not validation_queries:
        return {
            "key": "validation_queries",
            "score": 1,
            "comment": "No validation queries specified"
        }

    passed_count = 0
    failed_queries = []

    for vq in validation_queries:
        cypher = vq["cypher"]
        expected_results = vq["expected_results"]
        description = vq.get("description", "Query")

        try:
            actual_results = neo4j_client.query(cypher)

            # Compare results
            if _results_match(actual_results, expected_results):
                passed_count += 1
            else:
                failed_queries.append({
                    "description": description,
                    "expected": expected_results,
                    "actual": actual_results
                })
        except Exception as e:
            failed_queries.append({
                "description": description,
                "error": str(e)
            })

    score = passed_count / len(validation_queries)
    comment = f"Passed {passed_count}/{len(validation_queries)} validation queries"

    if failed_queries:
        first_failure = failed_queries[0]
        if "error" in first_failure:
            comment += f". Error: {first_failure['description']} - {first_failure['error']}"
        else:
            comment += f". Failed: {first_failure['description']}"

    return {
        "key": "validation_queries",
        "score": score,
        "comment": comment
    }


def no_duplicate_nodes_evaluator(run, example):
    """
    Check that no duplicate nodes exist for the same entity

    Critical for validating MERGE logic

    Args:
        run: LangSmith run object with outputs
        example: Dataset example with expected outputs

    Returns:
        Dict with score (binary 0/1) and feedback
    """
    expected_nodes = example.outputs.get("expected_nodes", [])

    if not expected_nodes:
        return {
            "key": "no_duplicates",
            "score": 1,
            "comment": "No nodes to check for duplicates"
        }

    duplicate_found = False
    duplicates = []

    for node in expected_nodes:
        label = node["label"]
        name = node["properties"]["name"]

        # Count nodes with same label and name
        query = f"""
        MATCH (n:{label} {{name: $name}})
        RETURN count(n) as count
        """

        try:
            result = neo4j_client.query(query, {"name": name})

            if result and result[0]['count'] > 1:
                duplicate_found = True
                duplicates.append(f"{label}:{name} ({result[0]['count']} copies)")
        except Exception as e:
            pass  # If query fails, node probably doesn't exist

    score = 0 if duplicate_found else 1

    if duplicate_found:
        comment = f"Duplicate nodes found: {', '.join(duplicates)}"
    else:
        comment = "No duplicate nodes - MERGE logic working correctly"

    return {
        "key": "no_duplicates",
        "score": score,
        "comment": comment
    }


def graph_metrics_evaluator(run, example):
    """
    Check overall graph metrics (node counts, relationship counts)

    Args:
        run: LangSmith run object with outputs
        example: Dataset example with expected outputs

    Returns:
        Dict with score (0-1) and feedback
    """
    expected_metrics = example.outputs.get("expected_metrics", {})

    if not expected_metrics:
        return {
            "key": "graph_metrics",
            "score": 1,
            "comment": "No metrics to validate"
        }

    actual_metrics = run.outputs.get("graph_data", {})

    correct_metrics = 0
    total_metrics = len(expected_metrics)
    metric_errors = []

    for metric_name, expected_value in expected_metrics.items():
        actual_value = actual_metrics.get(metric_name)

        # Allow some tolerance for metrics that might vary
        if metric_name in ["total_nodes", "total_relationships"]:
            # Exact match required
            if actual_value == expected_value:
                correct_metrics += 1
            else:
                metric_errors.append(
                    f"{metric_name}: expected {expected_value}, got {actual_value}"
                )
        else:
            # For other metrics, allow exact match or None
            if actual_value == expected_value or actual_value is None:
                correct_metrics += 1
            else:
                metric_errors.append(
                    f"{metric_name}: expected {expected_value}, got {actual_value}"
                )

    score = correct_metrics / total_metrics if total_metrics > 0 else 1
    comment = f"Correct metrics: {correct_metrics}/{total_metrics}"

    if metric_errors:
        comment += f". Errors: {metric_errors[0]}"
        if len(metric_errors) > 1:
            comment += f" and {len(metric_errors) - 1} more"

    return {
        "key": "graph_metrics",
        "score": score,
        "comment": comment
    }


def _results_match(actual: List[Dict], expected: List[Dict]) -> bool:
    """
    Compare query results with some flexibility

    Args:
        actual: Actual query results
        expected: Expected query results

    Returns:
        True if results match (exact or close enough)
    """
    if len(actual) != len(expected):
        return False

    # Try exact match first
    if actual == expected:
        return True

    # Try sorted match (order might differ)
    try:
        actual_sorted = sorted(actual, key=lambda x: str(x))
        expected_sorted = sorted(expected, key=lambda x: str(x))

        if actual_sorted == expected_sorted:
            return True
    except:
        pass

    # Try key-by-key comparison with some tolerance
    for i, expected_row in enumerate(expected):
        if i >= len(actual):
            return False

        actual_row = actual[i]

        for key, expected_value in expected_row.items():
            actual_value = actual_row.get(key)

            # Handle list comparisons (e.g., collect() results)
            if isinstance(expected_value, list) and isinstance(actual_value, list):
                if sorted(expected_value) != sorted(actual_value):
                    return False
            elif actual_value != expected_value:
                return False

    return True
