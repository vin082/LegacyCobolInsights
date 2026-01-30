"""
Upload evaluation datasets to LangSmith

This script uploads all test datasets to LangSmith for evaluation:
- COBOL-Graph-Builder-Tests
- COBOL-Parsing-Tests (future)
- COBOL-Retrieval-Tests (future)
- COBOL-Tech-Debt-Tests (future)

Usage:
    python -m evals.upload_datasets

    # Upload specific dataset only
    python -m evals.upload_datasets --dataset graph-builder
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
from evals.datasets.graph_test_cases import GRAPH_BUILDER_TEST_CASES
import argparse


def upload_graph_builder_dataset(client: Client, overwrite: bool = False):
    """
    Upload Graph Builder test dataset to LangSmith

    Args:
        client: LangSmith client
        overwrite: If True, delete existing dataset first
    """
    dataset_name = "COBOL-Graph-Builder-Tests"

    print(f"\n📤 Uploading {dataset_name}...")
    print(f"   Total test cases: {len(GRAPH_BUILDER_TEST_CASES)}")

    # Check if dataset exists
    try:
        existing = client.read_dataset(dataset_name=dataset_name)
        if existing:
            if overwrite:
                print(f"   ⚠️  Dataset exists - deleting and recreating...")
                client.delete_dataset(dataset_name=dataset_name)
            else:
                print(f"   ✅ Dataset already exists (use --overwrite to replace)")
                return
    except:
        pass  # Dataset doesn't exist

    # Create dataset
    dataset = client.create_dataset(
        dataset_name=dataset_name,
        description="Evaluation dataset for Graph Builder Agent - validates node/relationship creation accuracy"
    )

    print(f"   Created dataset: {dataset_name}")

    # Upload examples
    uploaded_count = 0
    for test_case in GRAPH_BUILDER_TEST_CASES:
        try:
            client.create_example(
                inputs=test_case["inputs"],
                outputs=test_case["outputs"],
                dataset_id=dataset.id,
                metadata=test_case.get("metadata", {})
            )
            uploaded_count += 1
        except Exception as e:
            print(f"   ❌ Error uploading {test_case['id']}: {e}")

    print(f"   ✅ Uploaded {uploaded_count}/{len(GRAPH_BUILDER_TEST_CASES)} examples")

    # Print breakdown by category
    categories = {}
    for tc in GRAPH_BUILDER_TEST_CASES:
        cat = tc["metadata"].get("category", "unknown")
        categories[cat] = categories.get(cat, 0) + 1

    print(f"\n   📊 Breakdown by category:")
    for cat, count in sorted(categories.items()):
        print(f"      {cat:.<20} {count} tests")


def main():
    """Upload datasets to LangSmith"""
    parser = argparse.ArgumentParser(description="Upload evaluation datasets to LangSmith")
    parser.add_argument(
        "--dataset",
        choices=["graph-builder", "all"],
        default="all",
        help="Which dataset to upload"
    )
    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Overwrite existing datasets"
    )
    args = parser.parse_args()

    print("=" * 70)
    print("UPLOAD EVALUATION DATASETS TO LANGSMITH")
    print("=" * 70)

    # Initialize LangSmith client
    try:
        client = Client()
        print(f"\n✅ Connected to LangSmith")
        print(f"   API endpoint: {client.api_url}")
    except Exception as e:
        print(f"\n❌ Error connecting to LangSmith: {e}")
        print("\nTroubleshooting:")
        print("1. Ensure LANGSMITH_API_KEY is set in environment")
        print("2. Check internet connection")
        print("3. Verify API key is valid")
        return

    # Upload datasets
    if args.dataset in ["graph-builder", "all"]:
        upload_graph_builder_dataset(client, overwrite=args.overwrite)

    # Future datasets
    if args.dataset == "all":
        print("\n📝 Note: Additional datasets (Parsing, Retrieval, Tech Debt) coming soon")

    print("\n" + "=" * 70)
    print("✅ DATASET UPLOAD COMPLETE")
    print("=" * 70)
    print("\n🔗 View datasets in LangSmith UI:")
    print("   https://smith.langchain.com")
    print("   Navigate to: Datasets → Your Project")
    print()


if __name__ == "__main__":
    main()
