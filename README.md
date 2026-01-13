# Legacy COBOL Insights

> An AI-powered knowledge graph system for analyzing and modernizing legacy COBOL codebases

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.11+](https://img.shields.io/badge/python-3.11+-blue.svg)](https://www.python.org/downloads/)
[![Neo4j](https://img.shields.io/badge/Neo4j-5.x-008CC1.svg)](https://neo4j.com/)
[![Multi-LLM](https://img.shields.io/badge/LLM-OpenAI%20%7C%20Groq%20%7C%20Gemini-purple.svg)](https://openai.com/)

## Overview

**Legacy COBOL Insights** is a production-ready multi-agent system that transforms legacy COBOL codebases into intelligent, queryable knowledge graphs. Built with LangGraph and powered by LLM agents, it provides deep insights into COBOL applications, helping organizations understand, maintain, and modernize their legacy systems.

### Key Capabilities

- **9 Specialized AI Agents** - Autonomous agents for ingestion, validation, parsing, enrichment, graph building, query generation, retrieval, documentation, and modernization
- **Multi-LLM Support** - Choose from OpenAI GPT-4/GPT-5, Groq (free & fast), or Google Gemini for cost-effective processing
- **Automated Documentation** - Generate comprehensive technical specs and business requirement documents in Markdown or DOCX
- **Modernization Recommendations** - AI-powered risk/value analysis with migration strategies and technology recommendations
- **LLM-Powered Analysis** - Semantic understanding of COBOL code with intelligent insights
- **Knowledge Graph** - Neo4j-based graph database for complex relationships
- **Natural Language Queries** - Ask questions in plain English, get Cypher queries automatically
- **Interactive Dashboard** - Streamlit UI for visualization and monitoring
- **Scalable Processing** - Handles repositories with 5,000+ COBOL files
- **Real-time Progress Tracking** - Monitor processing status at each stage

## Quick Start

### Prerequisites

- **Python 3.11+** - [Download](https://www.python.org/downloads/)
- **Neo4j 5.x** - [Install](https://neo4j.com/download/) or use Docker
- **LLM API Key** - Choose one:
  - [OpenAI API Key](https://platform.openai.com/api-keys) (GPT-4/GPT-5)
  - [Groq API Key](https://console.groq.com) (Free, fast Llama/Mixtral)
  - [Google API Key](https://aistudio.google.com/app/apikey) (Affordable Gemini)

### Installation

```bash
# Clone the repository
git clone https://github.com/vin082/LegacyCobolInsights.git
cd LegacyCobolInsights

# Create virtual environment
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies
cd cobol_agentic_kg
pip install -r requirements.txt

# Configure environment
cp .env.example .env
# Edit .env with your credentials
```

### Environment Configuration

Create a `.env` file with the following:

```env
# LLM Provider Configuration
# Options: "openai", "groq", or "google"
LLM_PROVIDER=openai

# OpenAI Configuration
OPENAI_API_KEY=sk-your-api-key-here
OPENAI_MODEL=gpt-4o-mini

# Groq Configuration (for cost savings)
# Get your API key from https://console.groq.com
GROQ_API_KEY=gsk_your-groq-api-key-here
GROQ_MODEL=llama-3.1-8b-instant

# Google Gemini Configuration (affordable & powerful)
# Get your API key from https://aistudio.google.com/app/apikey
GOOGLE_API_KEY=AIza-your-google-api-key-here
GEMINI_MODEL=gemini-2.5-flash

# Neo4j Configuration
NEO4J_URI=bolt://localhost:7687
NEO4J_USERNAME=neo4j
NEO4J_PASSWORD=your-neo4j-password

# LangSmith Configuration (optional - for tracing)
LANGCHAIN_TRACING_V2=true
LANGSMITH_API_KEY=lsv2_pt_your-api-key-here
LANGCHAIN_PROJECT=cobol-insights
```

### Start Neo4j

**Option 1: Docker** (Recommended)
```bash
docker run -d \
  --name neo4j \
  -p 7474:7474 -p 7687:7687 \
  -e NEO4J_AUTH=neo4j/your_password \
  neo4j:latest
```

**Option 2: Neo4j Desktop**
Download and install from [neo4j.com/download](https://neo4j.com/download/)

### Run the Application

```bash
cd cobol_agentic_kg
streamlit run ui/app.py
```

Open your browser to [http://localhost:8501](http://localhost:8501)

## Usage

### 1. Upload COBOL Files

**Via UI:**
- Drag and drop `.cob`, `.cbl`, or `.cobol` files
- Or provide a GitHub repository URL

**Via Python:**
```python
from workflows.orchestrator import CobolOrchestrator

orchestrator = CobolOrchestrator()
result = orchestrator.process_file("path/to/program.cob")
print(f"Program: {result['parsed_data']['program_name']}")
```

### 2. Monitor Processing

Track progress through 9 stages:
1. Ingestion - File upload and validation
2. Validation - COBOL syntax checking
3. Parsing - Structure extraction
4. Enrichment - LLM semantic analysis
5. Graph Building - Neo4j node creation
6. Query Generation - Natural language to Cypher
7. Retrieval - Execute queries
8. Documentation - Generate technical specs
9. Modernization - Analyze and recommend migration strategies

### 3. Query the Knowledge Graph

**Natural Language:**
```
"Show all programs with high complexity"
"Which programs read the CUSTOMER file?"
"List programs in the banking domain"
```

**Direct Cypher:**
```cypher
MATCH (p:Program)-[:READS]->(f:File {name: 'CUSTOMER'})
RETURN p.name, p.complexity
ORDER BY p.complexity DESC
```

### 4. Generate Documentation

**Via UI:**
- Select "Document Generator" from the sidebar
- Choose document type (System Overview, Program Details, etc.)
- Select format (Markdown or DOCX)
- Apply filters (max programs, complexity, domain)
- Click "Generate Document"

**Document Types:**
- **System Overview** - High-level architecture and statistics
- **Program Details** - Comprehensive technical specifications for each program
- **Dependency Map** - Call graphs and data flow analysis
- **Data Dictionary** - File and variable catalog

**Performance Tips:**
- Start with 5-10 programs for testing
- Filter by "High" complexity for critical programs
- Use domain filters to focus on specific business areas

### 5. Get Modernization Recommendations

**Via UI:**
- Select "Modernization" from the sidebar
- Configure filters (max programs, complexity, domain)
- Click "Run Modernization Analysis"
- Review priority-ranked recommendations with:
  - Risk scores (complexity, coupling, size)
  - Business value scores (usage, criticality)
  - Recommended strategies (Rewrite, Strangler Fig, Retire, Encapsulate)
  - Technology recommendations
  - Effort estimates
- Export recommendations as CSV

**Modernization Strategies:**
- **Rewrite** - High value, low risk programs
- **Strangler Fig Pattern** - High value, high risk programs (gradual migration)
- **Retire/Replace** - Low value, low risk programs
- **Encapsulate & Modernize** - Low value, high risk programs (wrap with APIs)

## Features in Detail

### Multi-Agent Architecture

| Agent | Responsibility | Technology |
|-------|---------------|------------|
| **Ingestion** | File upload, format detection | Python, pathlib |
| **Validation** | COBOL syntax validation | Regex, AST parsing |
| **Parsing** | Extract divisions, sections, variables | Custom parser |
| **Enrichment** | Semantic analysis, domain classification | LLM, LangChain |
| **Graph Builder** | Create nodes and relationships | Neo4j, Cypher |
| **Cypher Generator** | Convert NL queries to Cypher | LLM, few-shot prompting |
| **Retrieval** | Execute queries, format results | Neo4j driver |
| **Document Generator** | Create technical documentation | LLM, python-docx |
| **Modernization Advisor** | Risk/value analysis, migration strategies | LLM, scoring algorithms |

### Multi-LLM Provider Support

Switch between LLM providers based on your needs:

| Provider | Models | Best For | Cost |
|----------|--------|----------|------|
| **OpenAI** | GPT-4o, GPT-4o-mini, GPT-5 | High-quality analysis, complex reasoning | $$$ |
| **Groq** | Llama 3.1/3.3, Mixtral, Qwen | Fast inference, cost savings | FREE |
| **Google** | Gemini 2.5 Flash, Gemini 3 Pro | Balanced performance & cost | $ |

**Switching Providers:**
- Change `LLM_PROVIDER` in `.env` file
- Or use the UI dropdown to switch on-the-fly
- Each provider has its own API key configuration

### Knowledge Graph Schema

```
(Program)-[:CONTAINS]->(Division)
(Division)-[:CONTAINS]->(Section)
(Section)-[:CONTAINS]->(Paragraph)
(Program)-[:READS]->(File)
(Program)-[:WRITES]->(File)
(Program)-[:CALLS]->(Program)
(Program)-[:BELONGS_TO]->(Domain)
(Variable)-[:DECLARED_IN]->(Program)
(Variable)-[:USED_IN]->(Paragraph)
```

### Supported COBOL Elements

- Program identification
- Division structure (ID, ENV, DATA, PROCEDURE)
- File definitions (FD, SD)
- Variable declarations (01-49 levels)
- Paragraph and section definitions
- CALL statements (program dependencies)
- File I/O operations (READ, WRITE, OPEN, CLOSE)
- Complexity metrics (cyclomatic, lines of code)

## Testing

### Sample COBOL Repositories

Test the system with public COBOL codebases:

| Repository | Size | Description |
|------------|------|-------------|
| [COBOL Programming Course](https://github.com/openmainframeproject/cobol-programming-course) | 100+ files | Open Mainframe Project tutorials |
| [GnuCOBOL Contrib](https://github.com/OCamlPro/gnucobol-contrib) | 50+ files | Community contributions |
| [COBOL Examples](https://github.com/cschneid-the-elder/COBOL) | 10+ files | Simple examples |

### Run Tests

```bash
# Unit tests
pytest tests/

# Integration test with sample repo
python test_system.py

# Clear Neo4j database
python -c "from utils.neo4j_client import neo4j_client; neo4j_client.clear_cobol_data()"
```

## Performance

Tested on:
- **Dataset**: 5,000 COBOL files (~500K LOC)
- **Processing Time**: 4-5 minutes
- **LLM Calls**: ~10,000 (with caching)
- **Graph Size**: 50,000+ nodes, 200,000+ relationships
- **Query Response**: < 2 seconds
- **Documentation**: 2-4 minutes for 10 programs (adjustable)

## Configuration

Edit `config/settings.py`:

```python
# Processing settings
BATCH_SIZE = 10
MAX_WORKERS = 5
ENABLE_CACHING = True

# LLM settings
LLM_PROVIDER = "openai"  # or "groq", "google"
LLM_TEMPERATURE = 0.1
LLM_MAX_TOKENS = 2000

# Neo4j settings
MAX_CONNECTION_POOL_SIZE = 50
CONNECTION_TIMEOUT = 30
```

## Project Structure

```
LegacyCobolInsights/
└── cobol_agentic_kg/
    ├── agents/              # 9 specialized agents
    │   ├── ingestion.py
    │   ├── validation.py
    │   ├── parsing.py
    │   ├── enrichment.py
    │   ├── graph_builder.py
    │   ├── cypher_gen.py
    │   ├── retrieval.py
    │   ├── document_generator.py
    │   └── modernization.py  # NEW
    ├── workflows/           # LangGraph orchestration
    │   └── orchestrator.py
    ├── utils/               # Shared utilities
    │   ├── state.py
    │   ├── neo4j_client.py
    │   ├── llm_factory.py   # NEW: Multi-LLM support
    │   ├── logger.py
    │   └── cache.py
    ├── config/              # Configuration
    │   └── settings.py
    ├── ui/                  # Streamlit interface
    │   └── app.py
    ├── requirements.txt
    ├── .env.example
    ├── README.md
    ├── QUICKSTART.md
    └── SYSTEM_OVERVIEW.md
```

## Documentation

- [Quick Start Guide](cobol_agentic_kg/QUICKSTART.md)
- [System Overview](cobol_agentic_kg/SYSTEM_OVERVIEW.md)
- [Detailed README](cobol_agentic_kg/README.md)

## Security

- API keys are stored in `.env` (excluded from version control)
- Neo4j credentials are configurable
- No COBOL source code is stored in logs
- LLM calls are rate-limited and cached

## Contributing

Contributions are welcome! Please follow these steps:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- [LangChain](https://github.com/langchain-ai/langchain) - LLM framework
- [LangGraph](https://github.com/langchain-ai/langgraph) - Multi-agent orchestration
- [Neo4j](https://neo4j.com/) - Graph database
- [OpenAI](https://openai.com/) - Language models
- [Groq](https://groq.com/) - Fast LLM inference
- [Google](https://ai.google.dev/) - Gemini models
- [Streamlit](https://streamlit.io/) - UI framework
- [Open Mainframe Project](https://www.openmainframeproject.org/) - COBOL resources

## Contact

For questions or support, please open an issue on GitHub.

---

**Built with care for COBOL modernization**
