"""
Run all evaluations for COBOL Agentic KG System

This script runs all available evaluations and generates a summary report.

Usage:
    python -m evals.run_all_evals
"""
import sys
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from datetime import datetime


def main():
    """Run all evaluation scripts"""
    print("=" * 80)
    print("COBOL AGENTIC KG - COMPREHENSIVE EVALUATION SUITE")
    print("=" * 80)
    print(f"Started at: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print()

    results = {}

    # Eval 1: Cypher Generation
    print("📝 [1/2] Running Cypher Generation Evaluation...")
    print("-" * 80)
    try:
        from evals.eval_cypher_gen import main as run_cypher_eval
        run_cypher_eval()
        results["cypher_generation"] = "✅ PASSED"
    except Exception as e:
        results["cypher_generation"] = f"❌ FAILED: {str(e)}"
        print(f"Error: {e}")

    print()

    # Eval 2: Graph Builder
    print("🔨 [2/2] Running Graph Builder Evaluation...")
    print("-" * 80)
    try:
        from evals.eval_graph_builder import main as run_graph_eval
        run_graph_eval()
        results["graph_builder"] = "✅ PASSED"
    except Exception as e:
        results["graph_builder"] = f"❌ FAILED: {str(e)}"
        print(f"Error: {e}")

    print()
    print("=" * 80)
    print("EVALUATION SUMMARY")
    print("=" * 80)
    print()

    for eval_name, status in results.items():
        print(f"  {eval_name:.<40} {status}")

    print()
    print("=" * 80)
    print(f"Completed at: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print()
    print("🔗 View detailed results in LangSmith UI:")
    print("   https://smith.langchain.com")
    print("=" * 80)


if __name__ == "__main__":
    main()
