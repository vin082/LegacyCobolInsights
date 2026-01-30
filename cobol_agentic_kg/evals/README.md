# COBOL Agentic KG - Evaluation System

Production-grade evaluation framework for validating agent accuracy and reliability.

## Overview

This evaluation system provides quantifiable metrics for all agents in the COBOL Insights Knowledge Graph system:

- **Cypher Generation Agent**: Query generation accuracy ✅
- **Graph Builder Agent**: Node/relationship creation accuracy ✅
- **Parsing Agent**: Entity extraction accuracy *(coming soon)*
- **Retrieval Agent**: Query results and answer quality *(coming soon)*
- **Tech Debt Analyzer**: Debt scoring consistency *(coming soon)*

## 📁 Structure

```
evals/
├── __init__.py
├── datasets/
│   ├── graph_test_cases.py       # Graph Builder test cases (15 cases) ✅
│   ├── parsing_test_cases.py     # Coming soon
│   ├── retrieval_test_cases.py   # Coming soon
│   └── tech_debt_test_cases.py   # Coming soon
├── evaluators/
│   ├── custom_evaluators.py      # Cypher evaluators ✅
│   ├── graph_evaluators.py       # Graph Builder evaluators ✅
│   ├── parsing_evaluators.py     # Coming soon
│   ├── retrieval_evaluators.py   # Coming soon
│   └── tech_debt_evaluators.py   # Coming soon
├── eval_cypher_gen.py            # Cypher evaluation runner ✅
├── eval_graph_builder.py         # Graph Builder evaluation runner ✅
├── upload_datasets.py            # Dataset uploader ✅
├── run_all_evals.py              # Run all evaluations ✅
└── README.md
```

## 🚀 Quick Start

### 1. Upload Datasets to LangSmith

```bash
# Upload all datasets
python -m evals.upload_datasets

# Upload specific dataset
python -m evals.upload_datasets --dataset graph-builder

# Overwrite existing datasets
python -m evals.upload_datasets --overwrite
```

### 2. Run Evaluations

```bash
# Run all evaluations
python -m evals.run_all_evals

# Run specific agent evaluation
python -m evals.eval_graph_builder
python -m evals.eval_cypher_gen

# Run with category filter
python -m evals.eval_graph_builder --category basic
```

### 3. View Results

Results are automatically uploaded to LangSmith:
- Navigate to https://smith.langchain.com
- View project: `cobol-insights`
- Check experiments for detailed metrics

## Graph Builder Evaluation

### Test Coverage

**15 test cases across 6 categories:**

| Category | Tests | Description |
|----------|-------|-------------|
| Basic | 3 | Simple programs with minimal dependencies |
| Intermediate | 2 | Multiple calls, copybooks, data files |
| Complex | 2 | Real-world programs with all dependency types |
| Edge Cases | 3 | MERGE logic, cyclic dependencies, minimal data |
| JCL | 1 | Job processing with multiple steps |
| CardDemo | 1 | Real CardDemo program (COTRN00C) |

### Evaluator Metrics

Each test case is validated against 6 metrics:

1. **Node Existence** (0-1): All expected nodes created
2. **Node Properties** (0-1): Property values are correct
3. **Relationship Existence** (0-1): All relationships created with correct types/directions
4. **Validation Queries** (0-1): Custom Cypher queries validate graph patterns
5. **No Duplicate Nodes** (0/1): MERGE logic prevents duplicates
6. **Graph Metrics** (0-1): Overall node/relationship counts match

## Prerequisites

Ensure you have LangSmith configured in your `.env`:

```env
LANGCHAIN_TRACING_V2=true
LANGSMITH_API_KEY=lsv2_pt_...
LANGCHAIN_PROJECT=cobol-insights
```

### 2. Create Dataset in LangSmith

Go to https://smith.langchain.com and create a dataset named `Cobol-basic-tests` with test cases like:

**Example Input:**
```json
{
  "query": "Show me details about CUSTMAST program"
}
```

**Example Reference Output:**
```json
{
  "cypher_should_contain": ["MATCH", "CobolProgram", "CUSTMAST"],
  "cypher_valid": true
}
```

### 3. Run Evaluations

**Run specific eval:**
```bash
cd cobol_agentic_kg
python -m evals.eval_cypher_gen
```

**Run all evals:**
```bash
cd cobol_agentic_kg
python -m evals.run_all_evals
```

## 📊 Available Evaluations

### Cypher Generation (`eval_cypher_gen.py`)

Evaluates the Cypher Generator Agent's ability to translate natural language to valid Cypher queries.

**Metrics:**
- `cypher_valid` - Query is syntactically valid (0 or 1)
- `contains_keywords` - Contains expected keywords (0-1)
- `avoids_keywords` - Avoids incorrect keywords (0 or 1)

**Target Scores:**
- Validity: 100%
- Contains Keywords: >85%

## 🎯 Custom Evaluators

### Available Evaluators

1. **`cypher_validity_evaluator`**
   - Tests if Cypher query executes without syntax errors
   - Binary score (0 or 1)

2. **`cypher_contains_keywords_evaluator`**
   - Checks if query includes expected keywords from reference output
   - Expects `cypher_should_contain` list in reference outputs
   - Score: % of keywords found

3. **`cypher_avoids_keywords_evaluator`**
   - Checks query doesn't include wrong keywords
   - Expects `cypher_should_not_contain` list in reference outputs
   - Binary score (0 or 1)

4. **`groundedness_evaluator`**
   - Validates LLM outputs are grounded in source data
   - Expects `summary_should_mention` and `summary_should_avoid` lists
   - Score: average of mentions and avoidance

### Adding New Evaluators

Add your custom evaluator to `evaluators/custom_evaluators.py`:

```python
def my_custom_evaluator(run, example):
    """
    Your custom evaluation logic

    Args:
        run: LangSmith run object with outputs
        example: Dataset example with expected outputs

    Returns:
        Dict with key, score (0-1), and optional comment
    """
    output = run.outputs.get("your_field")
    expected = example.outputs.get("expected_value")

    score = 1.0 if output == expected else 0.0

    return {
        "key": "my_metric",
        "score": score,
        "comment": "Optional feedback"
    }
```

Then import and use it in your eval script.

## 📈 Viewing Results

After running evaluations:

1. Go to https://smith.langchain.com
2. Navigate to your project: `cobol-insights`
3. Click on **"Datasets & Testing"** tab
4. Select your dataset: `Cobol-basic-tests`
5. View experiment results with detailed traces

## 🔧 Troubleshooting

**Error: Dataset not found**
- Ensure dataset name matches exactly (case-sensitive)
- Check dataset exists in LangSmith UI

**Error: Module not found**
- Run from `cobol_agentic_kg` directory
- Use `python -m evals.eval_cypher_gen` format

**Error: Neo4j connection failed**
- Ensure Neo4j is running
- Check credentials in `.env`

## 📝 Best Practices

1. **Start Small**: Begin with 5-10 test cases
2. **Version Datasets**: Create `v1`, `v2` as your schema evolves
3. **Tag Experiments**: Use meaningful prefixes like `cypher-gen-v1.2`
4. **Monitor Trends**: Track eval scores over time
5. **Automate**: Run evals on every PR (see CI/CD section)

## 🤖 CI/CD Integration

Add to your GitHub Actions workflow:

```yaml
- name: Run Evaluations
  env:
    LANGSMITH_API_KEY: ${{ secrets.LANGSMITH_API_KEY }}
    LANGCHAIN_TRACING_V2: true
  run: |
    python -m evals.run_all_evals
```

## 📚 Resources

- [LangSmith Docs](https://docs.smith.langchain.com)
- [LangChain Evaluation Guide](https://python.langchain.com/docs/guides/evaluation)
- [LangGraph Testing](https://langchain-ai.github.io/langgraph/how-tos/testing/)
