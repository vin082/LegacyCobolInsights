# Quick Start Guide

## Prerequisites

1. **Neo4j Database**
   ```bash
   # Install Neo4j Desktop or run with Docker
   docker run -d \
     --name neo4j \
     -p 7474:7474 -p 7687:7687 \
     -e NEO4J_AUTH=neo4j/your_password \
     neo4j:latest
   ```

2. **Python 3.11+**
   ```bash
   python --version
   ```

3. **OpenAI API Key**
   - Get your key from https://platform.openai.com/api-keys

## Installation

1. **Clone/Navigate to project**
   ```bash
   cd cobol_agentic_kg
   ```

2. **Create virtual environment**
   ```bash
   python -m venv venv
   source venv/bin/activate  # On Windows: venv\Scripts\activate
   ```

3. **Install dependencies**
   ```bash
   pip install -r requirements.txt
   ```

4. **Configure environment**
   ```bash
   cp .env.example .env
   # Edit .env and add your credentials:
   # - OPENAI_API_KEY
   # - NEO4J_PASSWORD
   ```

## Running the Application

### Option 1: Streamlit UI (Recommended)

```bash
streamlit run ui/app.py
```

Then open your browser to http://localhost:8501

### Option 2: Python Script

Create a test script:

```python
from workflows.orchestrator import orchestrator

# Process a single file
result = orchestrator.process_file("path/to/your/program.cob")
print(f"Status: {result['status']}")
print(f"Program: {result['parsed_data']['program_name']}")

# Query the graph
query_result = orchestrator.query_graph("Show all programs")
print(f"Results: {query_result['query_results']}")
```

## Testing with Sample Data

1. **Clone a COBOL repository**
   ```bash
   git clone https://github.com/openmainframeproject/cobol-programming-course
   ```

2. **Process through UI**
   - Open Streamlit app
   - Go to "Clone Repository" page
   - Enter: `https://github.com/openmainframeproject/cobol-programming-course`
   - Click "Clone and Process"

3. **Query the data**
   - Go to "Query Graph" page
   - Try sample queries:
     - "Show all programs with high complexity"
     - "Which programs read customer files?"
     - "List programs by domain"

## Architecture Overview

```
User Upload → Ingestion Agent → Validation Agent → Parsing Agent
                                                        ↓
                                                   Enrichment Agent (LLM)
                                                        ↓
                                                   Graph Builder → Neo4j
                                                        ↓
User Query → Cypher Generator (LLM) → Retrieval Agent → Results
```

## Common Commands

```bash
# Run Streamlit app
streamlit run ui/app.py

# Run tests
pytest tests/

# Clear Neo4j graph
python -c "from utils.neo4j_client import neo4j_client; neo4j_client.clear_cobol_data()"

# Check graph statistics
python -c "from utils.neo4j_client import neo4j_client; print(neo4j_client.get_statistics())"
```

## Troubleshooting

### Connection Errors

```
Error: Failed to connect to Neo4j
```
**Solution**: Verify Neo4j is running and credentials are correct in `.env`

### LLM Errors

```
Error: OpenAI API key not found
```
**Solution**: Add `OPENAI_API_KEY` to `.env` file

### Import Errors

```
ModuleNotFoundError: No module named 'langchain'
```
**Solution**: Activate virtual environment and run `pip install -r requirements.txt`

## Next Steps

1. Process your own COBOL repository
2. Explore the Analytics page for insights
3. Build custom queries
4. Export data for reporting

## Sample Repositories for Testing

- **Small** (< 10 files): https://github.com/cschneid-the-elder/COBOL
- **Medium** (10-50 files): https://github.com/OCamlPro/gnucobol-contrib
- **Large** (100+ files): https://github.com/openmainframeproject/cobol-programming-course

## Support

For issues, check:
- Logs in terminal where Streamlit is running
- Neo4j browser at http://localhost:7474
- `errors` field in processing results
