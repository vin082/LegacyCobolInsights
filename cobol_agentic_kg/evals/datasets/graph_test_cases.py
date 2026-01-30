"""
Test dataset for Graph Builder Agent evaluation

This dataset contains test cases for validating:
- Node creation accuracy
- Relationship creation accuracy
- Property assignment correctness
- MERGE logic (no duplicates)
- Graph consistency
"""

GRAPH_BUILDER_TEST_CASES = [
    # ========================================================================
    # BASIC TESTS - Simple programs with minimal dependencies
    # ========================================================================
    {
        "id": "simple_program_no_dependencies",
        "inputs": {
            "parsed_data": {
                "program_name": "SIMPLE01",
                "program_id": "SIMPLE01",
                "file_type": "COBOL_PROGRAM",
                "loc": 100,
                "domain": "Utilities",
                "description": "Simple utility program",
                "business_logic": "Performs basic calculations",
                "calls": [],
                "copybooks": [],
                "files_read": [],
                "files_written": [],
                "complexity_score": 20
            }
        },
        "outputs": {
            "expected_nodes": [
                {
                    "label": "CobolProgram",
                    "properties": {
                        "name": "SIMPLE01",
                        "loc": 100,
                        "domain": "Utilities",
                        "complexity_score": 20
                    }
                }
            ],
            "expected_relationships": [],
            "validation_queries": [
                {
                    "description": "SIMPLE01 exists with correct properties",
                    "cypher": "MATCH (p:CobolProgram {name: 'SIMPLE01'}) RETURN p.loc as loc, p.domain as domain, p.complexity_score as complexity",
                    "expected_results": [{"loc": 100, "domain": "Utilities", "complexity": 20}]
                },
                {
                    "description": "No duplicate SIMPLE01 nodes",
                    "cypher": "MATCH (p:CobolProgram {name: 'SIMPLE01'}) RETURN count(p) as count",
                    "expected_results": [{"count": 1}]
                }
            ],
            "expected_metrics": {
                "total_nodes": 1,
                "total_relationships": 0
            }
        },
        "metadata": {
            "category": "basic",
            "test_type": "fresh_ingestion"
        }
    },

    {
        "id": "simple_program_single_call",
        "inputs": {
            "parsed_data": {
                "program_name": "CALLER01",
                "file_type": "COBOL_PROGRAM",
                "loc": 150,
                "domain": "Business Logic",
                "calls": ["CALLED01"],
                "copybooks": [],
                "files_read": [],
                "files_written": [],
                "complexity_score": 35
            }
        },
        "outputs": {
            "expected_nodes": [
                {"label": "CobolProgram", "properties": {"name": "CALLER01"}},
                {"label": "CobolProgram", "properties": {"name": "CALLED01"}}
            ],
            "expected_relationships": [
                {"type": "CALLS", "from": "CALLER01", "to": "CALLED01"}
            ],
            "validation_queries": [
                {
                    "description": "CALLER01 calls CALLED01",
                    "cypher": "MATCH (p1:CobolProgram {name: 'CALLER01'})-[:CALLS]->(p2:CobolProgram {name: 'CALLED01'}) RETURN p2.name as called",
                    "expected_results": [{"called": "CALLED01"}]
                },
                {
                    "description": "CALLS relationship count is 1",
                    "cypher": "MATCH (p:CobolProgram {name: 'CALLER01'})-[:CALLS]->() RETURN count(*) as count",
                    "expected_results": [{"count": 1}]
                }
            ]
        },
        "metadata": {
            "category": "basic",
            "test_type": "fresh_ingestion"
        }
    },

    {
        "id": "program_with_copybook",
        "inputs": {
            "parsed_data": {
                "program_name": "PROG01",
                "file_type": "COBOL_PROGRAM",
                "loc": 200,
                "domain": "Data Processing",
                "calls": [],
                "copybooks": ["DATACOPY"],
                "files_read": [],
                "files_written": [],
                "complexity_score": 40
            }
        },
        "outputs": {
            "expected_nodes": [
                {"label": "CobolProgram", "properties": {"name": "PROG01"}},
                {"label": "Copybook", "properties": {"name": "DATACOPY"}}
            ],
            "expected_relationships": [
                {"type": "INCLUDES", "from": "PROG01", "to": "DATACOPY"}
            ],
            "validation_queries": [
                {
                    "description": "PROG01 includes DATACOPY",
                    "cypher": "MATCH (p:CobolProgram {name: 'PROG01'})-[:INCLUDES]->(c:Copybook {name: 'DATACOPY'}) RETURN c.name as copybook",
                    "expected_results": [{"copybook": "DATACOPY"}]
                }
            ]
        },
        "metadata": {
            "category": "basic",
            "test_type": "fresh_ingestion"
        }
    },

    # ========================================================================
    # INTERMEDIATE TESTS - Multiple dependencies
    # ========================================================================
    {
        "id": "program_multiple_calls",
        "inputs": {
            "parsed_data": {
                "program_name": "MAIN01",
                "file_type": "COBOL_PROGRAM",
                "loc": 500,
                "domain": "Main Processing",
                "calls": ["SUB01", "SUB02", "SUB03"],
                "copybooks": ["MAINCOPY"],
                "files_read": [],
                "files_written": [],
                "complexity_score": 60
            }
        },
        "outputs": {
            "expected_nodes": [
                {"label": "CobolProgram", "properties": {"name": "MAIN01"}},
                {"label": "CobolProgram", "properties": {"name": "SUB01"}},
                {"label": "CobolProgram", "properties": {"name": "SUB02"}},
                {"label": "CobolProgram", "properties": {"name": "SUB03"}},
                {"label": "Copybook", "properties": {"name": "MAINCOPY"}}
            ],
            "expected_relationships": [
                {"type": "CALLS", "from": "MAIN01", "to": "SUB01"},
                {"type": "CALLS", "from": "MAIN01", "to": "SUB02"},
                {"type": "CALLS", "from": "MAIN01", "to": "SUB03"},
                {"type": "INCLUDES", "from": "MAIN01", "to": "MAINCOPY"}
            ],
            "validation_queries": [
                {
                    "description": "MAIN01 has 3 outgoing CALLS relationships",
                    "cypher": "MATCH (p:CobolProgram {name: 'MAIN01'})-[:CALLS]->() RETURN count(*) as count",
                    "expected_results": [{"count": 3}]
                },
                {
                    "description": "MAIN01 has 1 INCLUDES relationship",
                    "cypher": "MATCH (p:CobolProgram {name: 'MAIN01'})-[:INCLUDES]->() RETURN count(*) as count",
                    "expected_results": [{"count": 1}]
                }
            ]
        },
        "metadata": {
            "category": "intermediate",
            "test_type": "fresh_ingestion"
        }
    },

    {
        "id": "program_with_data_files",
        "inputs": {
            "parsed_data": {
                "program_name": "FILEIO01",
                "file_type": "COBOL_PROGRAM",
                "loc": 300,
                "domain": "File Processing",
                "calls": [],
                "copybooks": [],
                "files_read": ["INPUT-FILE"],
                "files_written": ["OUTPUT-FILE"],
                "complexity_score": 45
            }
        },
        "outputs": {
            "expected_nodes": [
                {"label": "CobolProgram", "properties": {"name": "FILEIO01"}},
                {"label": "DataFile", "properties": {"name": "INPUT-FILE"}},
                {"label": "DataFile", "properties": {"name": "OUTPUT-FILE"}}
            ],
            "expected_relationships": [
                {"type": "READS", "from": "FILEIO01", "to": "INPUT-FILE"},
                {"type": "WRITES", "from": "FILEIO01", "to": "OUTPUT-FILE"}
            ],
            "validation_queries": [
                {
                    "description": "FILEIO01 reads INPUT-FILE",
                    "cypher": "MATCH (p:CobolProgram {name: 'FILEIO01'})-[:READS]->(f:DataFile {name: 'INPUT-FILE'}) RETURN f.name as file",
                    "expected_results": [{"file": "INPUT-FILE"}]
                },
                {
                    "description": "FILEIO01 writes OUTPUT-FILE",
                    "cypher": "MATCH (p:CobolProgram {name: 'FILEIO01'})-[:WRITES]->(f:DataFile {name: 'OUTPUT-FILE'}) RETURN f.name as file",
                    "expected_results": [{"file": "OUTPUT-FILE"}]
                }
            ]
        },
        "metadata": {
            "category": "intermediate",
            "test_type": "fresh_ingestion"
        }
    },

    # ========================================================================
    # COMPLEX TESTS - Real-world scenarios
    # ========================================================================
    {
        "id": "complex_program_all_dependencies",
        "inputs": {
            "parsed_data": {
                "program_name": "CBACT01C",
                "file_type": "COBOL_PROGRAM",
                "loc": 1250,
                "domain": "Customer Account",
                "description": "Customer account management",
                "business_logic": "Manages customer account operations including balance updates",
                "calls": ["CBACT02C", "UTILITY01", "ERRLOG"],
                "copybooks": ["CUSTCOPY", "ACTCOPY"],
                "files_read": ["CUSTOMER-MASTER"],
                "files_written": ["ACCOUNT-FILE"],
                "complexity_score": 78
            }
        },
        "outputs": {
            "expected_nodes": [
                {"label": "CobolProgram", "properties": {"name": "CBACT01C", "loc": 1250, "complexity_score": 78}},
                {"label": "CobolProgram", "properties": {"name": "CBACT02C"}},
                {"label": "CobolProgram", "properties": {"name": "UTILITY01"}},
                {"label": "CobolProgram", "properties": {"name": "ERRLOG"}},
                {"label": "Copybook", "properties": {"name": "CUSTCOPY"}},
                {"label": "Copybook", "properties": {"name": "ACTCOPY"}},
                {"label": "DataFile", "properties": {"name": "CUSTOMER-MASTER"}},
                {"label": "DataFile", "properties": {"name": "ACCOUNT-FILE"}}
            ],
            "expected_relationships": [
                {"type": "CALLS", "from": "CBACT01C", "to": "CBACT02C"},
                {"type": "CALLS", "from": "CBACT01C", "to": "UTILITY01"},
                {"type": "CALLS", "from": "CBACT01C", "to": "ERRLOG"},
                {"type": "INCLUDES", "from": "CBACT01C", "to": "CUSTCOPY"},
                {"type": "INCLUDES", "from": "CBACT01C", "to": "ACTCOPY"},
                {"type": "READS", "from": "CBACT01C", "to": "CUSTOMER-MASTER"},
                {"type": "WRITES", "from": "CBACT01C", "to": "ACCOUNT-FILE"}
            ],
            "validation_queries": [
                {
                    "description": "All 3 CALLS relationships exist",
                    "cypher": "MATCH (p:CobolProgram {name: 'CBACT01C'})-[:CALLS]->() RETURN count(*) as count",
                    "expected_results": [{"count": 3}]
                },
                {
                    "description": "All 2 INCLUDES relationships exist",
                    "cypher": "MATCH (p:CobolProgram {name: 'CBACT01C'})-[:INCLUDES]->() RETURN count(*) as count",
                    "expected_results": [{"count": 2}]
                },
                {
                    "description": "File I/O relationships captured",
                    "cypher": "MATCH (p:CobolProgram {name: 'CBACT01C'})-[:READS|WRITES]->(f:DataFile) RETURN count(f) as count",
                    "expected_results": [{"count": 2}]
                },
                {
                    "description": "Total outgoing relationships is 7",
                    "cypher": "MATCH (p:CobolProgram {name: 'CBACT01C'})-[r]->() RETURN count(r) as count",
                    "expected_results": [{"count": 7}]
                }
            ]
        },
        "metadata": {
            "category": "complex",
            "test_type": "fresh_ingestion"
        }
    },

    {
        "id": "shared_copybook_multiple_programs",
        "inputs": {
            "parsed_data": {
                "programs": [
                    {
                        "program_name": "PROG_A",
                        "copybooks": ["SHARED_COPY"],
                        "loc": 200,
                        "complexity_score": 30
                    },
                    {
                        "program_name": "PROG_B",
                        "copybooks": ["SHARED_COPY"],
                        "loc": 250,
                        "complexity_score": 35
                    }
                ]
            }
        },
        "outputs": {
            "validation_queries": [
                {
                    "description": "Only ONE SHARED_COPY node exists",
                    "cypher": "MATCH (c:Copybook {name: 'SHARED_COPY'}) RETURN count(c) as count",
                    "expected_results": [{"count": 1}]
                },
                {
                    "description": "SHARED_COPY has 2 incoming INCLUDES relationships",
                    "cypher": "MATCH ()-[:INCLUDES]->(c:Copybook {name: 'SHARED_COPY'}) RETURN count(*) as count",
                    "expected_results": [{"count": 2}]
                },
                {
                    "description": "Both programs include the same copybook instance",
                    "cypher": "MATCH (p:CobolProgram)-[:INCLUDES]->(c:Copybook {name: 'SHARED_COPY'}) RETURN collect(p.name) as programs",
                    "expected_results": [{"programs": ["PROG_A", "PROG_B"]}]
                }
            ]
        },
        "metadata": {
            "category": "complex",
            "test_type": "shared_dependency"
        }
    },

    # ========================================================================
    # EDGE CASES - MERGE logic, re-ingestion, updates
    # ========================================================================
    {
        "id": "reingest_same_program_merge_test",
        "inputs": {
            "first_ingestion": {
                "program_name": "MERGE_TEST",
                "file_type": "COBOL_PROGRAM",
                "loc": 200,
                "complexity_score": 50,
                "calls": ["PROGA"],
                "copybooks": [],
                "files_read": [],
                "files_written": []
            },
            "second_ingestion": {
                "program_name": "MERGE_TEST",  # Same program
                "file_type": "COBOL_PROGRAM",
                "loc": 250,  # Updated LOC
                "complexity_score": 55,  # Updated complexity
                "calls": ["PROGA", "PROGB"],  # Added new call
                "copybooks": ["NEWCOPY"],  # Added copybook
                "files_read": [],
                "files_written": []
            }
        },
        "outputs": {
            "validation_queries": [
                {
                    "description": "Only ONE node for MERGE_TEST (no duplicates)",
                    "cypher": "MATCH (p:CobolProgram {name: 'MERGE_TEST'}) RETURN count(p) as count",
                    "expected_results": [{"count": 1}]
                },
                {
                    "description": "Properties updated to latest values",
                    "cypher": "MATCH (p:CobolProgram {name: 'MERGE_TEST'}) RETURN p.loc as loc, p.complexity_score as complexity",
                    "expected_results": [{"loc": 250, "complexity": 55}]
                },
                {
                    "description": "Both calls exist after re-ingestion",
                    "cypher": "MATCH (p:CobolProgram {name: 'MERGE_TEST'})-[:CALLS]->() RETURN count(*) as count",
                    "expected_results": [{"count": 2}]
                },
                {
                    "description": "New copybook relationship added",
                    "cypher": "MATCH (p:CobolProgram {name: 'MERGE_TEST'})-[:INCLUDES]->(c:Copybook {name: 'NEWCOPY'}) RETURN c.name as copybook",
                    "expected_results": [{"copybook": "NEWCOPY"}]
                }
            ]
        },
        "metadata": {
            "category": "edge_case",
            "test_type": "merge_update"
        }
    },

    {
        "id": "cyclic_dependency",
        "inputs": {
            "parsed_data": {
                "programs": [
                    {
                        "program_name": "PROG_X",
                        "calls": ["PROG_Y"],
                        "loc": 300,
                        "complexity_score": 40
                    },
                    {
                        "program_name": "PROG_Y",
                        "calls": ["PROG_X"],  # Cyclic!
                        "loc": 350,
                        "complexity_score": 45
                    }
                ]
            }
        },
        "outputs": {
            "validation_queries": [
                {
                    "description": "PROG_X calls PROG_Y",
                    "cypher": "MATCH (x:CobolProgram {name: 'PROG_X'})-[:CALLS]->(y:CobolProgram {name: 'PROG_Y'}) RETURN y.name as called",
                    "expected_results": [{"called": "PROG_Y"}]
                },
                {
                    "description": "PROG_Y calls PROG_X (cyclic)",
                    "cypher": "MATCH (y:CobolProgram {name: 'PROG_Y'})-[:CALLS]->(x:CobolProgram {name: 'PROG_X'}) RETURN x.name as called",
                    "expected_results": [{"called": "PROG_X"}]
                },
                {
                    "description": "Both programs exist without duplication",
                    "cypher": "MATCH (p:CobolProgram) WHERE p.name IN ['PROG_X', 'PROG_Y'] RETURN count(p) as count",
                    "expected_results": [{"count": 2}]
                }
            ]
        },
        "metadata": {
            "category": "edge_case",
            "test_type": "cyclic_dependency"
        }
    },

    {
        "id": "empty_program_minimal_data",
        "inputs": {
            "parsed_data": {
                "program_name": "EMPTY01",
                "file_type": "COBOL_PROGRAM",
                "loc": 50,
                "calls": [],
                "copybooks": [],
                "files_read": [],
                "files_written": [],
                "complexity_score": 10,
                "description": "",
                "business_logic": ""
            }
        },
        "outputs": {
            "expected_nodes": [
                {"label": "CobolProgram", "properties": {"name": "EMPTY01", "loc": 50, "complexity_score": 10}}
            ],
            "expected_relationships": [],
            "validation_queries": [
                {
                    "description": "EMPTY01 exists as isolated node",
                    "cypher": "MATCH (p:CobolProgram {name: 'EMPTY01'}) RETURN p.name as name, p.loc as loc",
                    "expected_results": [{"name": "EMPTY01", "loc": 50}]
                },
                {
                    "description": "EMPTY01 has no outgoing relationships",
                    "cypher": "MATCH (p:CobolProgram {name: 'EMPTY01'})-[r]->() RETURN count(r) as count",
                    "expected_results": [{"count": 0}]
                }
            ]
        },
        "metadata": {
            "category": "edge_case",
            "test_type": "minimal_data"
        }
    },

    # ========================================================================
    # JCL TESTS - Different file type
    # ========================================================================
    {
        "id": "jcl_job_with_steps",
        "inputs": {
            "parsed_data": {
                "job_name": "BILLJOB1",
                "file_type": "JCL",
                "job_steps": [
                    {"step_name": "STEP01", "program": "CBACT01C"},
                    {"step_name": "STEP02", "program": "CBACT02C"}
                ],
                "input_datasets": ["INPUT.DATA"],
                "output_datasets": ["OUTPUT.DATA"]
            }
        },
        "outputs": {
            "expected_nodes": [
                {"label": "Job", "properties": {"name": "BILLJOB1"}},
                {"label": "JobStep", "properties": {"name": "STEP01"}},
                {"label": "JobStep", "properties": {"name": "STEP02"}},
                {"label": "CobolProgram", "properties": {"name": "CBACT01C"}},
                {"label": "CobolProgram", "properties": {"name": "CBACT02C"}},
                {"label": "Dataset", "properties": {"name": "INPUT.DATA"}},
                {"label": "Dataset", "properties": {"name": "OUTPUT.DATA"}}
            ],
            "validation_queries": [
                {
                    "description": "Job has 2 steps",
                    "cypher": "MATCH (j:Job {name: 'BILLJOB1'})-[:HAS_STEP]->(s:JobStep) RETURN count(s) as count",
                    "expected_results": [{"count": 2}]
                },
                {
                    "description": "Steps execute programs",
                    "cypher": "MATCH (s:JobStep)-[:EXECUTES]->(p:CobolProgram) WHERE s.name IN ['STEP01', 'STEP02'] RETURN count(p) as count",
                    "expected_results": [{"count": 2}]
                }
            ]
        },
        "metadata": {
            "category": "jcl",
            "test_type": "job_processing"
        }
    },

    # ========================================================================
    # REAL-WORLD CARDDEMO EXAMPLES
    # ========================================================================
    {
        "id": "carddemo_cotrn00c",
        "inputs": {
            "parsed_data": {
                "program_name": "COTRN00C",
                "file_type": "COBOL_PROGRAM",
                "loc": 423,
                "domain": "Transaction Processing",
                "description": "Transaction menu program",
                "calls": ["COTRN01C", "COTRN02C"],
                "copybooks": ["COCOM01Y", "COTTL01Y", "CSDAT01Y"],
                "files_read": [],
                "files_written": [],
                "complexity_score": 55
            }
        },
        "outputs": {
            "expected_nodes": [
                {"label": "CobolProgram", "properties": {"name": "COTRN00C", "loc": 423}},
                {"label": "CobolProgram", "properties": {"name": "COTRN01C"}},
                {"label": "CobolProgram", "properties": {"name": "COTRN02C"}},
                {"label": "Copybook", "properties": {"name": "COCOM01Y"}},
                {"label": "Copybook", "properties": {"name": "COTTL01Y"}},
                {"label": "Copybook", "properties": {"name": "CSDAT01Y"}}
            ],
            "validation_queries": [
                {
                    "description": "COTRN00C has 2 program calls",
                    "cypher": "MATCH (p:CobolProgram {name: 'COTRN00C'})-[:CALLS]->() RETURN count(*) as count",
                    "expected_results": [{"count": 2}]
                },
                {
                    "description": "COTRN00C includes 3 copybooks",
                    "cypher": "MATCH (p:CobolProgram {name: 'COTRN00C'})-[:INCLUDES]->(c:Copybook) RETURN count(c) as count",
                    "expected_results": [{"count": 3}]
                }
            ]
        },
        "metadata": {
            "category": "carddemo",
            "test_type": "real_program"
        }
    }
]


def get_test_cases_by_category(category: str = None):
    """Filter test cases by category"""
    if category is None:
        return GRAPH_BUILDER_TEST_CASES
    return [tc for tc in GRAPH_BUILDER_TEST_CASES if tc["metadata"]["category"] == category]


def get_test_case_by_id(test_id: str):
    """Get a specific test case by ID"""
    for tc in GRAPH_BUILDER_TEST_CASES:
        if tc["id"] == test_id:
            return tc
    return None
