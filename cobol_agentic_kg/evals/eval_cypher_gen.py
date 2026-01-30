"""
Evaluation script for Cypher Generation Agent

This script evaluates the Cypher Generator Agent's ability to:
1. Generate syntactically valid Cypher queries
2. Include expected keywords and patterns
3. Produce queries that return reasonable results

Usage:
    python -m evals.eval_cypher_gen
"""
import sys
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from langsmith import Client
from langsmith.evaluation import evaluate
from agents.cypher_gen import cypher_generator_agent
from evals.evaluators.custom_evaluators import (
    cypher_validity_evaluator,
    cypher_contains_keywords_evaluator,
    cypher_avoids_keywords_evaluator
)


def run_cypher_generation(inputs: dict) -> dict:
    """
    Target function for evaluation - runs cypher generation agent

    Args:
        inputs: Dict with "query" key containing natural language query

    Returns:
        Dict with generated Cypher query
    """
    query = inputs.get("query", "")

    # Create state for agent
    state = {
        "user_query": query
    }

    # Run agent
    result = cypher_generator_agent.process(state)

    return {
        "cypher": result.get("generated_cypher", ""),
        "status": result.get("status", "unknown")
    }


def main():
    """Run Cypher generation evaluation"""
    print("=" * 70)
    print("CYPHER GENERATION AGENT EVALUATION")
    print("=" * 70)
    print()

    # Initialize LangSmith client
    client = Client()

    # Define dataset name
    dataset_name = "Cobol-Basic-Tests"

    print(f"📊 Running evaluation on dataset: {dataset_name}")
    print(f"🎯 Evaluating: Cypher Generation Agent")
    print()

    # Run evaluation
    try:
        results = evaluate(
            run_cypher_generation,
            data=dataset_name,
            evaluators=[
                cypher_validity_evaluator,
                cypher_contains_keywords_evaluator,
                cypher_avoids_keywords_evaluator
            ],
            experiment_prefix="cypher-gen",
            description="Evaluate Cypher generation from natural language queries"
        )

        print()
        print("=" * 70)
        print("✅ EVALUATION COMPLETE")
        print("=" * 70)
        print()
        print("📈 Results Summary:")

        # ExperimentResults is an object with properties, not a dict
        print(f"   Experiment Name: {results.experiment_name}")

        # Get aggregate scores directly from results object
        try:
            # Try using the aggregate_feedback method if available
            if hasattr(results, 'aggregate_feedback'):
                aggregate = results.aggregate_feedback()
                print(f"   Total Examples: {aggregate.get('n', 'N/A')}")
                print()
                print("📊 Aggregate Scores:")
                for key, value in aggregate.items():
                    if key != 'n' and isinstance(value, (int, float)):
                        print(f"   {key:.<30} {value:.2%}")
            else:
                # Fallback: count results
                results_list = list(results)
                print(f"   Total Examples: {len(results_list)}")
                print()
                print("📊 Evaluator Metrics:")
                print("   (Detailed scores available in LangSmith UI)")
        except Exception as e:
            print(f"   Total Examples: Completed successfully")
            print()
            print("📊 Evaluation Metrics:")
            print("   ✅ Cypher Validity")
            print("   ✅ Keyword Matching")
            print("   ✅ Keyword Avoidance")
            print()
            print("   (View detailed scores in LangSmith UI below)")

        print()
        print("🔗 View detailed results in LangSmith UI:")
        print(f"   https://smith.langchain.com")
        print(f"   Project: cobol-insights")
        print(f"   Experiment: {results.experiment_name}")
        print()

    except Exception as e:
        print(f"❌ Error running evaluation: {e}")
        print()
        print("Troubleshooting:")
        print("1. Ensure dataset 'Cobol-Basic-Tests' exists in LangSmith")
        print("2. Check that LANGSMITH_API_KEY is set correctly")
        print("3. Verify Neo4j is running and accessible")
        raise


if __name__ == "__main__":
    main()
