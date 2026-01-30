"""
Evaluation script for Graph Builder Agent

This script evaluates the Graph Builder Agent's ability to:
1. Create correct nodes with proper labels and properties
2. Create correct relationships with proper types and directions
3. Implement MERGE logic correctly (no duplicates on re-ingestion)
4. Maintain graph consistency
5. Handle edge cases (cyclic dependencies, shared resources, etc.)

Usage:
    python -m evals.eval_graph_builder

    # Or with specific test category
    python -m evals.eval_graph_builder --category basic
"""
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent))

# Fix encoding for Windows console
if sys.platform == 'win32':
    sys.stdout.reconfigure(encoding='utf-8')

# Import settings first to load environment variables
from config.settings import settings

from langsmith import Client
from langsmith.evaluation import evaluate
from agents.graph_builder import graph_builder_agent
from utils.neo4j_client import neo4j_client
from evals.evaluators.graph_evaluators import (
    node_existence_evaluator,
    node_properties_evaluator,
    relationship_existence_evaluator,
    validation_query_evaluator,
    no_duplicate_nodes_evaluator,
    graph_metrics_evaluator
)
import argparse


def run_graph_builder(inputs: dict) -> dict:
    """
    Target function for evaluation - runs graph builder agent

    Handles different test types:
    - fresh_ingestion: Normal program ingestion
    - merge_update: Re-ingest same program to test MERGE logic
    - shared_dependency: Multiple programs sharing copybooks/files
    - cyclic_dependency: Programs with circular calls

    Args:
        inputs: Dict with parsed_data and metadata

    Returns:
        Dict with graph_data metrics and status
    """
    metadata = inputs.get("metadata", {})
    test_type = metadata.get("test_type", "fresh_ingestion")

    # Handle merge/update tests (two ingestions)
    if test_type == "merge_update" and "first_ingestion" in inputs:
        # First ingestion
        first_state = _create_state_from_parsed_data(inputs["first_ingestion"], "first")
        graph_builder_agent.process(first_state)

        # Second ingestion (update)
        second_state = _create_state_from_parsed_data(inputs["second_ingestion"], "second")
        result = graph_builder_agent.process(second_state)

        return {
            "graph_data": result.get("graph_data", {}),
            "status": result.get("status", "unknown")
        }

    # Handle tests with multiple programs
    elif "programs" in inputs.get("parsed_data", {}):
        programs = inputs["parsed_data"]["programs"]

        # Ingest each program
        for i, prog_data in enumerate(programs):
            state = _create_state_from_parsed_data(prog_data, f"prog_{i}")
            result = graph_builder_agent.process(state)

        return {
            "graph_data": result.get("graph_data", {}),
            "status": result.get("status", "unknown")
        }

    # Handle JCL jobs
    elif inputs.get("parsed_data", {}).get("file_type") == "JCL":
        jcl_data = inputs["parsed_data"]
        state = {
            "file_id": f"test_jcl_{jcl_data.get('job_name', 'unknown')}",
            "file_path": f"/test/{jcl_data.get('job_name', 'unknown')}.jcl",
            "file_type": "JCL",
            "jcl_data": jcl_data,
            "stage": "graph_building",
            "status": "pending",
            "errors": []
        }
        result = graph_builder_agent.process(state)

        return {
            "graph_data": result.get("graph_data", {}),
            "status": result.get("status", "unknown")
        }

    # Normal single program ingestion
    else:
        parsed_data = inputs.get("parsed_data", {})
        state = _create_state_from_parsed_data(parsed_data, "main")
        result = graph_builder_agent.process(state)

        return {
            "graph_data": result.get("graph_data", {}),
            "status": result.get("status", "unknown"),
            "program_name": parsed_data.get("program_name", "")
        }


def _create_state_from_parsed_data(parsed_data: dict, id_suffix: str) -> dict:
    """
    Create state object from parsed data

    Args:
        parsed_data: Parsed COBOL program data
        id_suffix: Suffix for unique file ID

    Returns:
        State dict for graph builder agent
    """
    program_name = parsed_data.get("program_name", "unknown")

    return {
        "file_id": f"test_{program_name}_{id_suffix}",
        "file_path": f"/test/{program_name}.cbl",
        "file_type": parsed_data.get("file_type", "COBOL_PROGRAM"),
        "file_metadata": {
            "filename": f"{program_name}.cbl",
            "size": 0,
            "encoding": "utf-8"
        },
        "parsed_data": parsed_data,
        "enriched_data": {},
        "file_content": "",
        "stage": "graph_building",
        "status": "pending",
        "errors": []
    }


def cleanup_test_nodes(test_cases: list):
    """
    Clean up test nodes from Neo4j after evaluation

    Important: Call this to ensure test isolation

    Args:
        test_cases: List of test case dicts
    """
    print("\n🧹 Cleaning up test nodes...")

    node_names = set()

    # Collect all node names from test cases
    for tc in test_cases:
        # Regular parsed_data
        if "parsed_data" in tc["inputs"]:
            parsed = tc["inputs"]["parsed_data"]

            if "program_name" in parsed:
                node_names.add(parsed["program_name"])
            if "job_name" in parsed:
                node_names.add(parsed["job_name"])

            # Add dependencies
            for call in parsed.get("calls", []):
                node_names.add(call)
            for cb in parsed.get("copybooks", []):
                node_names.add(cb)
            for df in parsed.get("data_files", []):
                if isinstance(df, dict):
                    node_names.add(df.get("name"))
                else:
                    node_names.add(df)

            # Handle multiple programs
            if "programs" in parsed:
                for prog in parsed["programs"]:
                    node_names.add(prog.get("program_name"))
                    for cb in prog.get("copybooks", []):
                        node_names.add(cb)

        # Merge test cases
        if "first_ingestion" in tc["inputs"]:
            node_names.add(tc["inputs"]["first_ingestion"]["program_name"])
        if "second_ingestion" in tc["inputs"]:
            node_names.add(tc["inputs"]["second_ingestion"]["program_name"])

    # Delete nodes
    deleted_count = 0
    for name in node_names:
        if name:
            query = "MATCH (n {name: $name}) DETACH DELETE n"
            try:
                neo4j_client.query(query, {"name": name})
                deleted_count += 1
            except Exception as e:
                pass  # Node might not exist

    print(f"   Deleted {deleted_count} test nodes")


def main():
    """Run Graph Builder evaluation"""
    parser = argparse.ArgumentParser(description="Evaluate Graph Builder Agent")
    parser.add_argument(
        "--category",
        choices=["basic", "intermediate", "complex", "edge_case", "jcl", "carddemo"],
        help="Run only tests in specific category"
    )
    parser.add_argument(
        "--cleanup-only",
        action="store_true",
        help="Just clean up test nodes without running evaluation"
    )
    args = parser.parse_args()

    print("=" * 70)
    print("GRAPH BUILDER AGENT EVALUATION")
    print("=" * 70)
    print()

    # Initialize LangSmith client
    client = Client()

    # Define dataset name
    dataset_name = "COBOL-Graph-Builder-Tests"

    # Clean up before evaluation (if requested)
    if args.cleanup_only:
        from evals.datasets.graph_test_cases import GRAPH_BUILDER_TEST_CASES
        cleanup_test_nodes(GRAPH_BUILDER_TEST_CASES)
        print("\n✅ Cleanup complete")
        return

    print(f"📊 Running evaluation on dataset: {dataset_name}")
    if args.category:
        print(f"🏷️  Category filter: {args.category}")
    print(f"🎯 Evaluating: Graph Builder Agent")
    print()

    # Run evaluation
    try:
        results = evaluate(
            run_graph_builder,
            data=dataset_name,
            evaluators=[
                node_existence_evaluator,
                node_properties_evaluator,
                relationship_existence_evaluator,
                validation_query_evaluator,
                no_duplicate_nodes_evaluator,
                graph_metrics_evaluator
            ],
            experiment_prefix="graph-builder",
            description="Evaluate graph construction accuracy and consistency"
        )

        print()
        print("=" * 70)
        print("✅ EVALUATION COMPLETE")
        print("=" * 70)
        print()
        print(f"📈 Results Summary:")
        print(f"   Experiment Name: {results.experiment_name}")
        print()

        # Try to get aggregate feedback
        try:
            if hasattr(results, 'aggregate_feedback'):
                aggregate = results.aggregate_feedback()
                print(f"   Total Examples: {aggregate.get('n', 'N/A')}")
                print()
                print("📊 Aggregate Scores:")
                for key, value in aggregate.items():
                    if key != 'n' and isinstance(value, (int, float)):
                        print(f"   {key:.<35} {value:.2%}")
            else:
                results_list = list(results)
                print(f"   Total Examples: {len(results_list)}")
                print()
                print("📊 Evaluator Metrics:")
                print("   ✅ Node Existence")
                print("   ✅ Node Properties")
                print("   ✅ Relationship Existence")
                print("   ✅ Validation Queries")
                print("   ✅ No Duplicate Nodes")
                print("   ✅ Graph Metrics")
        except Exception as e:
            print(f"   Total Examples: Completed successfully")
            print()
            print("📊 Evaluation Metrics:")
            print("   ✅ Node Existence - Verifies all expected nodes created")
            print("   ✅ Node Properties - Validates property values")
            print("   ✅ Relationship Existence - Checks all relationships")
            print("   ✅ Validation Queries - Custom graph pattern tests")
            print("   ✅ No Duplicate Nodes - MERGE logic verification")
            print("   ✅ Graph Metrics - Overall graph statistics")
            print()
            print("   (View detailed scores in LangSmith UI below)")

        print()
        print("🔗 View detailed results in LangSmith UI:")
        print(f"   https://smith.langchain.com")
        print(f"   Project: cobol-insights")
        print(f"   Experiment: {results.experiment_name}")
        print()

        # Clean up test nodes after evaluation
        print()
        from evals.datasets.graph_test_cases import GRAPH_BUILDER_TEST_CASES, get_test_cases_by_category

        if args.category:
            test_cases = get_test_cases_by_category(args.category)
        else:
            test_cases = GRAPH_BUILDER_TEST_CASES

        cleanup_test_nodes(test_cases)
        print()

    except Exception as e:
        print(f"❌ Error running evaluation: {e}")
        print()
        print("Troubleshooting:")
        print(f"1. Ensure dataset '{dataset_name}' exists in LangSmith")
        print("2. Check that LANGSMITH_API_KEY is set correctly")
        print("3. Verify Neo4j is running and accessible")
        print("4. Run 'python -m evals.upload_datasets' to create dataset")
        print()
        import traceback
        traceback.print_exc()
        raise


if __name__ == "__main__":
    main()
