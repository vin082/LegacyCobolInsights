"""
Test script for COBOL Agentic KG System
Run this to verify your installation
"""
import tempfile
from workflows.orchestrator import orchestrator
from utils.neo4j_client import neo4j_client
from utils.logger import logger

# Sample COBOL program for testing
SAMPLE_COBOL = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       AUTHOR. TEST-USER.
       DATE-WRITTEN. 2024-01-01.
      *****************************************************************
      * TEST PROGRAM FOR AGENTIC SYSTEM                              *
      * DEMONSTRATES FILE OPERATIONS AND PROGRAM CALLS               *
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO CUSTFILE.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD.
           05  CUST-ID              PIC 9(8).
           05  CUST-NAME            PIC X(50).

       WORKING-STORAGE SECTION.
       01  WS-COUNT                 PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-FILES
           PERFORM PROCESS-RECORDS
           CALL 'SUBPROG1'
           CALL 'SUBPROG2'
           STOP RUN.

       INITIALIZE-FILES.
           OPEN INPUT CUSTOMER-FILE.

       PROCESS-RECORDS.
           READ CUSTOMER-FILE
           WRITE CUSTOMER-RECORD.
"""


def test_neo4j_connection():
    """Test 1: Neo4j connection"""
    print("\n" + "="*80)
    print("TEST 1: Neo4j Connection")
    print("="*80)

    try:
        if neo4j_client.health_check():
            print("✅ Neo4j connection successful")
            return True
        else:
            print("❌ Neo4j connection failed")
            return False
    except Exception as e:
        print(f"❌ Error: {e}")
        return False


def test_file_processing():
    """Test 2: Process sample COBOL file"""
    print("\n" + "="*80)
    print("TEST 2: File Processing")
    print("="*80)

    try:
        # Create temporary file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.cob', delete=False) as f:
            f.write(SAMPLE_COBOL)
            temp_path = f.name

        # Process file
        result = orchestrator.process_file(temp_path)

        # Check result
        if result['status'] == 'completed':
            print(f"✅ File processed successfully")
            print(f"   Program: {result['parsed_data']['program_name']}")
            print(f"   Calls: {result['parsed_data']['calls']}")
            print(f"   Files: {result['parsed_data']['files_declared']}")
            print(f"   Procedures: {len(result['parsed_data']['procedures'])}")
            print(f"   Summary: {result['enriched_data'].get('summary', 'N/A')}")
            print(f"   Domain: {result['enriched_data'].get('business_domain', 'N/A')}")
            return True
        else:
            print(f"❌ Processing failed: {result.get('errors', [])}")
            return False

    except Exception as e:
        print(f"❌ Error: {e}")
        return False
    finally:
        import os
        if 'temp_path' in locals():
            os.unlink(temp_path)


def test_graph_creation():
    """Test 3: Verify graph was created"""
    print("\n" + "="*80)
    print("TEST 3: Graph Creation")
    print("="*80)

    try:
        stats = neo4j_client.get_statistics()

        print(f"   Programs: {stats['nodes'].get('CobolProgram', 0)}")
        print(f"   Files: {stats['nodes'].get('DataFile', 0)}")
        print(f"   Procedures: {stats['nodes'].get('Procedure', 0)}")
        print(f"   Total Relationships: {stats['total_relationships']}")

        if stats['total_nodes'] > 0:
            print("✅ Graph created successfully")
            return True
        else:
            print("❌ No nodes found in graph")
            return False

    except Exception as e:
        print(f"❌ Error: {e}")
        return False


def test_query_execution():
    """Test 4: Execute a query"""
    print("\n" + "="*80)
    print("TEST 4: Query Execution")
    print("="*80)

    try:
        # Execute query
        result = orchestrator.query_graph("Show all programs")

        print(f"   Generated Cypher: {result['generated_cypher']}")
        print(f"   Results: {len(result['query_results'])} records")

        if result['query_results']:
            print(f"   Sample result: {result['query_results'][0]}")
            print("✅ Query executed successfully")
            return True
        else:
            print("❌ No query results")
            return False

    except Exception as e:
        print(f"❌ Error: {e}")
        return False


def main():
    """Run all tests"""
    print("\n" + "="*80)
    print("🧪 COBOL AGENTIC KG SYSTEM - TEST SUITE")
    print("="*80)

    tests = [
        ("Neo4j Connection", test_neo4j_connection),
        ("File Processing", test_file_processing),
        ("Graph Creation", test_graph_creation),
        ("Query Execution", test_query_execution),
    ]

    results = []
    for test_name, test_func in tests:
        try:
            result = test_func()
            results.append((test_name, result))
        except Exception as e:
            logger.error(f"Test {test_name} crashed: {e}")
            results.append((test_name, False))

    # Summary
    print("\n" + "="*80)
    print("📊 TEST SUMMARY")
    print("="*80)

    passed = sum(1 for _, result in results if result)
    total = len(results)

    for test_name, result in results:
        status = "✅ PASS" if result else "❌ FAIL"
        print(f"{status} - {test_name}")

    print("\n" + "="*80)
    print(f"TOTAL: {passed}/{total} tests passed")
    print("="*80)

    if passed == total:
        print("\n🎉 All tests passed! System is ready to use.")
        print("\nNext steps:")
        print("1. Run: streamlit run ui/app.py")
        print("2. Open browser to http://localhost:8501")
        print("3. Upload COBOL files or clone a repository")
    else:
        print("\n⚠️  Some tests failed. Please check:")
        print("1. Neo4j is running (bolt://localhost:7687)")
        print("2. Environment variables in .env are set")
        print("3. Dependencies are installed (pip install -r requirements.txt)")


if __name__ == "__main__":
    main()
