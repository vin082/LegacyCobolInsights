---
name: document-generation
description: Runtime instructions for generating technical documentation from the COBOL Knowledge Graph. Use this skill whenever the user requests document generation, program reports, system overviews, or any output derived from Neo4j KG data via the DocumentGeneratorAgent.
---

# Document Generation Skill

This skill guides document generation from the COBOL Knowledge Graph. It covers query strategies, graph schema, enrichment-aware logic, output structure, and export formats used by `DocumentGeneratorAgent`.

---

## 1. Knowledge Graph Schema Reference

All data is stored in Neo4j. Use these node labels, properties, and relationships when constructing Cypher queries for documentation.

### Node Types

| Label | Key Properties | Description |
|-------|---------------|-------------|
| `CobolProgram` | `name`, `domain`, `description`, `complexity_score`, `loc`, `code`, `business_logic`, `file_path` | Main COBOL program unit |
| `Procedure` | `name`, `type`, `description` | Paragraph or section within a program |
| `DataFile` | `name`, `description` | External file accessed by programs |
| `Copybook` | `name` | COBOL copybook (shared data definitions) |
| `Job` | `name`, `description` | JCL job definition |
| `Transaction` | `name` | CICS transaction |
| `ScreenMap` | `name` | BMS screen definition |
| `Variable` | `name`, `type`, `level` | Data item declared in a program |

### Relationship Types

| Relationship | From ’ To | Meaning |
|--------------|-----------|---------|
| `CALLS` | CobolProgram ’ CobolProgram | Program calls another program |
| `READS` | CobolProgram ’ DataFile | Program reads a data file |
| `WRITES` | CobolProgram ’ DataFile | Program writes to a data file |
| `CONTAINS` | CobolProgram ’ Procedure | Program contains a procedure/paragraph |
| `INCLUDES` | CobolProgram ’ Copybook | Program includes a copybook |
| `DECLARES_VARIABLE` | CobolProgram ’ Variable | Program declares a variable |
| `EXECUTES` | Job ’ CobolProgram | JCL job executes a program |
| `ALLOCATES` | Job ’ DataFile | JCL job allocates a dataset |
| `INVOKES` | Transaction ’ CobolProgram | CICS transaction invokes a program |

---

## 2. Document Types & When to Use Them

| doc_type | Use When | Key Data Sources |
|----------|----------|-----------------|
| `system_overview` | User wants a full catalog or summary of all/filtered programs | All CobolProgram nodes + aggregated stats |
| `program_detail` | User asks about a specific program | Single CobolProgram + its dependencies, data flows, procedures, business logic |

---

## 3. Core Query Strategies

### 3.1 Gathering Program Data (System Overview)

Use this pattern to fetch programs with aggregated metrics in a single query. Apply `WHERE` filters for domain/complexity, and always include a `LIMIT` (default 10) for performance:

```cypher
MATCH (p:CobolProgram)
// Optional filters: WHERE p.domain = 'Banking' AND p.complexity_score >= 30
OPTIONAL MATCH (p)-[:CALLS]->(called:CobolProgram)
OPTIONAL MATCH (caller:CobolProgram)-[:CALLS]->(p)
OPTIONAL MATCH (p)-[:CONTAINS]->(proc:Procedure)
OPTIONAL MATCH (p)-[r]->(f:DataFile)
WHERE type(r) IN ['READS', 'WRITES']

WITH p,
     COUNT(DISTINCT called) AS calls_out,
     COUNT(DISTINCT caller) AS calls_in,
     COUNT(DISTINCT proc) AS procedure_count,
     COUNT(DISTINCT CASE WHEN type(r) = 'READS' THEN f END) AS files_read,
     COUNT(DISTINCT CASE WHEN type(r) = 'WRITES' THEN f END) AS files_written

RETURN p.name AS program_name,
       COALESCE(p.domain, 'Not Enriched') AS domain,
       p.description AS description,
       COALESCE(p.complexity_score, 0) AS complexity,
       COALESCE(p.loc, 0) AS loc,
       calls_out, calls_in, procedure_count, files_read, files_written
ORDER BY COALESCE(p.complexity_score, calls_in + calls_out) DESC
LIMIT 10
```

### 3.2 Deep Dive into a Single Program

Run these queries together for a full program profile:

```cypher
// Program metadata
MATCH (p:CobolProgram {name: 'PROGRAM_NAME'})
RETURN p.name, p.domain, p.description, p.complexity_score, p.loc, p.business_logic

// Outbound calls
MATCH (p:CobolProgram {name: 'PROGRAM_NAME'})-[:CALLS]->(called:CobolProgram)
RETURN called.name, called.domain, called.complexity_score

// Inbound calls
MATCH (caller:CobolProgram)-[:CALLS]->(p:CobolProgram {name: 'PROGRAM_NAME'})
RETURN caller.name, caller.domain

// Data file operations
MATCH (p:CobolProgram {name: 'PROGRAM_NAME'})-[r]->(f:DataFile)
WHERE type(r) IN ['READS', 'WRITES']
RETURN f.name, type(r) AS operation, f.description

// Internal procedures
MATCH (p:CobolProgram {name: 'PROGRAM_NAME'})-[:CONTAINS]->(proc:Procedure)
RETURN proc.name, proc.type, proc.description
ORDER BY proc.name LIMIT 100

// Copybooks included
MATCH (p:CobolProgram {name: 'PROGRAM_NAME'})-[:INCLUDES]->(c:Copybook)
RETURN c.name

// JCL jobs that execute this program
MATCH (j:Job)-[:EXECUTES]->(p:CobolProgram {name: 'PROGRAM_NAME'})
RETURN j.name, j.description
```

### 3.3 System-Level Statistics

```cypher
// Node counts by label
MATCH (n)
WHERE n:CobolProgram OR n:Procedure OR n:DataFile OR n:Variable
RETURN labels(n)[0] AS label, count(*) AS count

// Relationship counts
MATCH ()-[r]->()
WHERE type(r) IN ['CALLS', 'READS', 'WRITES', 'CONTAINS_PROCEDURE', 'DECLARES_VARIABLE']
RETURN type(r) AS relationship, count(*) AS count
```

---

## 4. Enrichment-Aware Documentation Logic

Programs may or may not have been enriched by the EnrichmentAgent. Documentation quality depends on this:

| Enrichment Status | Available Data | Documentation Approach |
|-------------------|---------------|----------------------|
| **Enriched** (`domain` is NOT null / `Not Enriched`) | domain, description, business_logic, complexity_score, loc | Use all metadata directly in documentation |
| **Not Enriched** (`domain` is null or `Not Enriched`) | name, loc, raw source code (`p.code`) | Fall back to analyzing `p.code` (first ~2000 chars) to infer domain, complexity, and business logic |

**Always check enrichment status before generating per-program content.** If not enriched and source code is available, include it in the LLM prompt so it can infer context.

---

## 5. LLM Prompt Guidelines for Documentation

When invoking the LLM to generate per-program documentation, structure the prompt as:

1. **Role**: "You are a technical writer creating COBOL program documentation for developers and business analysts."
2. **Context block**: Include program name, domain, description, business_logic (or source code snippet if not enriched), LOC, and complexity.
3. **Graph data block**: List calls out, calls in, data files, procedures  as extracted from Neo4j.
4. **Section scaffolding**: Ask for these sections in order:
   - Purpose and Overview (2-3 sentences)
   - Business Logic (rules and processes)
   - Key Functionality (bullet points)
   - Data Operations (files read/written and why)
   - Dependencies (upstream and downstream programs)
   - Technical Notes (complexity warnings, maintenance concerns)
5. **Format**: Request markdown output. Keep it technical but clear.

**LLM Configuration**: Use `temperature=0.3` for consistent, factual documentation output. The LLM is initialized via `get_llm()` from `utils/llm_factory.py`, which respects the provider set in settings (`openai`, `groq`, or `google`).

---

## 6. Complexity-Based Warnings

Apply these thresholds when generating documentation to flag risky programs:

| Complexity Score | Risk Level | Action in Document |
|-----------------|------------|-------------------|
| < 30 | Low | No warning needed |
| 3069 | Medium | Note moderate complexity |
| >= 70 | High | Add a warning section advising refactoring or testing |

---

## 7. Output Structure

### Markdown (`doc_format: markdown`)

Use this document skeleton for `system_overview`:

```markdown
# COBOL System Technical Documentation
**Generated:** <timestamp>
**Total Programs Documented:** N
**Enriched Programs:** X / N

---
## 1. System Overview
- Purpose statement
- System statistics table (programs, data files, procedures)

## 2. Program Catalog
- Summary table: name, domain, complexity, LOC, calls out/in, files read/written

## 3. Detailed Program Specifications
- One subsection per program (### 3.1 PROGRAM_NAME)
- Generated by LLM (or fallback basic template if LLM fails)
```

For `program_detail`, output a single program's full specification (sections from the LLM prompt above).

### DOCX Export

DOCX export converts the generated markdown to a Word document:
- `#` ’ Title (centered)
- `##` ’ Heading 1
- `###` ’ Heading 2
- `####` ’ Heading 3
- Markdown tables ’ Word tables (styled `Light Grid Accent 1`, header row bolded)
- Bullet lists ’ Word bullet lists
- Code blocks ’ Courier New, 9pt, indented
- Blockquotes (`>`) ’ Indented, italic, gray text
- Emojis are stripped from headings and table cells for clean DOCX output

---

## 8. Filtering Programs

The `filters` dict supports these keys when gathering programs:

| Filter Key | Values | Effect |
|-----------|--------|--------|
| `domain` | Any string (e.g. `'Banking'`, `'Claims'`) | Filters by `p.domain` |
| `complexity` | `'low'`, `'medium'`, `'high'` | Maps to score thresholds: <30, 3069, >=70 |
| `max_programs` | Integer (default 10) | Limits result count for performance |

---

## 9. Fallback & Error Handling

- If LLM invocation fails for a program, fall back to a **basic template** that populates sections purely from Neo4j data (no LLM needed).
- If Neo4j queries fail, return empty lists/dicts  do not crash; log the error and continue with partial data.
- Always include a generation timestamp and enrichment status warning in the final document.

---

## 10. Key Files

| File | Role |
|------|------|
| [agents/document_generator.py](cobol_agentic_kg/agents/document_generator.py) | Main DocumentGeneratorAgent  orchestrates data gathering, LLM generation, and export |
| [utils/neo4j_client.py](cobol_agentic_kg/utils/neo4j_client.py) | Singleton Neo4j client; use `neo4j_client.query(cypher)` |
| [utils/llm_factory.py](cobol_agentic_kg/utils/llm_factory.py) | Multi-provider LLM factory; use `get_llm(temperature=...)` |
| [utils/state.py](cobol_agentic_kg/utils/state.py) | `DocumentGenerationState` TypedDict for state shape |
| [config/settings.py](cobol_agentic_kg/config/settings.py) | All config: LLM provider, Neo4j URI, filter defaults |
| [agents/graph_builder.py](cobol_agentic_kg/agents/graph_builder.py) | Defines what nodes/relationships get created (schema source of truth) |
| [agents/enrichment.py](cobol_agentic_kg/agents/enrichment.py) | Populates domain, business_logic, complexity_score on CobolProgram nodes |
