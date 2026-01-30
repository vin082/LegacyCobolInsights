# COBOL Knowledge Graph Skill

This skill provides commands and workflows for the COBOL Agentic Knowledge Graph project.

## Project Overview

This project builds a knowledge graph from legacy COBOL codebases using:
- **Neo4j** for graph storage
- **LangChain/LangGraph** for agent orchestration
- **Multiple LLM providers** (OpenAI, Groq, Gemini)

## Quick Reference Commands

### Check Neo4j Connection & Counts
```bash
uv run python -c "
from cobol_agentic_kg.utils.neo4j_client import Neo4jClient
client = Neo4jClient()
result = client.run_query('MATCH (n) RETURN labels(n)[0] as label, count(*) as count ORDER BY count DESC')
for r in result: print(f'{r[\"label\"]}: {r[\"count\"]}')
client.close()
"
```

### Common Cypher Queries

**List all programs:**
```cypher
MATCH (p:Program) RETURN p.name, p.type, p.complexity ORDER BY p.name
```

**Find program dependencies:**
```cypher
MATCH (p:Program)-[:CALLS]->(called:Program)
WHERE p.name = 'PROGRAM_NAME'
RETURN p.name, called.name
```

**Find data flow:**
```cypher
MATCH (p:Program)-[:USES]->(d:DataItem)
WHERE p.name = 'PROGRAM_NAME'
RETURN d.name, d.type, d.level
```

**Find JCL jobs for a program:**
```cypher
MATCH (j:Job)-[:EXECUTES]->(p:Program)
WHERE p.name = 'PROGRAM_NAME'
RETURN j.name, j.description
```

**Find copybooks used:**
```cypher
MATCH (p:Program)-[:INCLUDES]->(c:Copybook)
WHERE p.name = 'PROGRAM_NAME'
RETURN c.name
```

### Run the Streamlit UI
```bash
uv run streamlit run cobol_agentic_kg/ui/app.py
```

### Run Tests
```bash
uv run pytest cobol_agentic_kg/tests/ -v
```

### Run Evals
```bash
uv run python cobol_agentic_kg/evals/run_all_evals.py
```

## Debugging Common Issues

### Neo4j Connection Failed
1. Check Neo4j is running: `neo4j status` or check Docker
2. Verify `.env` has correct `NEO4J_URI`, `NEO4J_USER`, `NEO4J_PASSWORD`
3. Default URI: `bolt://localhost:7687`

### LLM API Errors
1. Check `.env` for `OPENAI_API_KEY`, `GROQ_API_KEY`, or `GOOGLE_API_KEY`
2. For Groq (free tier): Use `llama-3.1-70b-versatile` model
3. Test connection: `uv run python cobol_agentic_kg/test_groq_setup.py`

### Parser Issues
- Check `cobol_agentic_kg/agents/parsing.py` for COBOL parser
- Check `cobol_agentic_kg/agents/jcl_parser.py` for JCL parser
- Check `cobol_agentic_kg/agents/copybook_parser.py` for copybook parser

## Project Structure

```
cobol_agentic_kg/
├── agents/           # LangChain agents
│   ├── parsing.py        # COBOL parser
│   ├── jcl_parser.py     # JCL parser
│   ├── copybook_parser.py # Copybook parser
│   ├── validation.py     # Validation agent
│   ├── enrichment.py     # Enrichment agent
│   ├── graph_builder.py  # Neo4j graph builder
│   ├── cypher_gen.py     # Cypher query generator
│   ├── retrieval.py      # RAG retrieval
│   ├── tech_debt_analyzer.py # Tech debt analysis
│   ├── modernization.py  # Modernization recommendations
│   └── translation.py    # Code translation
├── config/
│   └── settings.py       # Configuration
├── utils/
│   ├── neo4j_client.py   # Neo4j connection
│   ├── llm_factory.py    # Multi-LLM support
│   └── state.py          # LangGraph state
├── workflows/
│   └── orchestrator.py   # Pipeline orchestration
├── ui/
│   └── app.py            # Streamlit UI
└── evals/                # Evaluation suite
```

## Key Files to Check

When debugging or making changes:
1. **Pipeline issues**: `workflows/orchestrator.py`
2. **Graph schema**: `agents/graph_builder.py`
3. **Query generation**: `agents/cypher_gen.py`
4. **State management**: `utils/state.py`
5. **UI problems**: `ui/app.py`
