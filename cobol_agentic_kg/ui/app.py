"""
Streamlit UI for COBOL Agentic Knowledge Graph System
"""
import streamlit as st
import sys
import os
from pathlib import Path
import time
import pandas as pd
from git import Repo
import tempfile
import shutil

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from workflows.orchestrator import orchestrator
from utils.neo4j_client import neo4j_client
from config.settings import settings
from utils.cache import get_cache
from utils.llm_factory import get_available_models, set_llm_provider
from agents.document_generator import document_generator_agent
from agents.modernization import ModernizationAgent
from agents.translation import code_translation_agent, LANGUAGE_CONFIG

# Page configuration
st.set_page_config(
    page_title=settings.app_title,
    page_icon=settings.app_icon,
    layout="wide",
    initial_sidebar_state="expanded"
)

# Custom CSS
st.markdown("""
<style>
    .main-header {
        font-size: 2.5rem;
        font-weight: bold;
        margin-bottom: 1rem;
    }
    .metric-card {
        background-color: #f0f2f6;
        padding: 1rem;
        border-radius: 0.5rem;
        margin-bottom: 1rem;
    }
    .status-success {
        color: #28a745;
    }
    .status-failed {
        color: #dc3545;
    }
    .status-processing {
        color: #ffc107;
    }
</style>
""", unsafe_allow_html=True)


def init_session_state():
    """Initialize session state variables"""
    if 'processing_results' not in st.session_state:
        st.session_state.processing_results = []
    if 'query_history' not in st.session_state:
        st.session_state.query_history = []


def show_sidebar():
    """Render sidebar with navigation and settings"""
    with st.sidebar:
        st.markdown("## Navigation")

        page = st.radio(
            "Select Page",
            ["📊 Dashboard", "📁 Upload Files", "🌐 Clone Repository", "🔍 Query Graph", "📈 Analytics", "📄 Documentation", "🔧 Modernization", "🔄 Translation"],
            label_visibility="collapsed"
        )

        st.markdown("---")

        # LLM Provider Selection
        st.markdown("### 🤖 LLM Configuration")

        available = get_available_models()
        current_provider = settings.llm_provider

        # Provider selection
        provider_options = ["openai", "groq", "google"]
        try:
            current_index = provider_options.index(current_provider)
        except ValueError:
            current_index = 0

        provider = st.selectbox(
            "Provider",
            options=provider_options,
            index=current_index,
            help="Select LLM provider: OpenAI (paid), Groq (free & fast), Google Gemini (affordable)"
        )

        # Model selection based on provider
        if provider != current_provider:
            set_llm_provider(provider)
            st.rerun()

        # Get available models for selected provider
        models_info = get_available_models()
        current_model = settings.get_llm_model()

        model = st.selectbox(
            "Model",
            options=models_info['models'],
            index=models_info['models'].index(current_model) if current_model in models_info['models'] else 0,
            help=f"Available models for {provider}"
        )

        if model != current_model:
            set_llm_provider(provider, model)
            st.success(f"✅ Switched to {model}")
            st.info("💡 Cache is provider-specific. Switching models will generate fresh responses.")

        # Show cost indicator
        if provider == "groq":
            st.caption("💰 FREE - Fast inference")
        elif provider == "google":
            st.caption("💵 Affordable - Gemini pricing")
        else:
            st.caption("💳 Paid - OpenAI pricing")

        st.markdown("---")

        # Neo4j connection status
        st.markdown("### System Status")

        if neo4j_client.health_check():
            st.success("✅ Neo4j Connected")
        else:
            st.error("❌ Neo4j Disconnected")

        # Graph statistics
        stats = neo4j_client.get_statistics()
        st.metric("Total Programs", stats['nodes'].get('CobolProgram', 0))
        st.metric("Total Relationships", stats['total_relationships'])

        st.markdown("---")

        # Cache statistics
        if settings.cache_enabled:
            st.markdown("### Cache")
            try:
                cache = get_cache()
                cache_stats = cache.get_stats()

                if cache_stats.get("connected"):
                    st.caption(f"Backend: {cache_stats.get('backend', 'N/A')}")
                    st.metric("Cached Queries", cache_stats.get('keys', 0))

                    # Clear cache button
                    if st.button("🗑️ Clear Cache", use_container_width=True):
                        cleared = cache.clear("query:*")
                        st.success(f"Cleared {cleared} cached queries!")
                        st.rerun()
                else:
                    st.warning("Cache disconnected")
            except Exception as e:
                st.error(f"Cache error: {e}")

        st.markdown("---")

        # Clear graph button
        if st.button("🗑️ Clear Graph", use_container_width=True):
            with st.spinner("Clearing graph..."):
                neo4j_client.clear_cobol_data()
                st.success("Graph cleared!")
                st.rerun()

    return page


def show_dashboard():
    """Show dashboard with overview statistics"""
    st.markdown('<div class="main-header">🕸️ COBOL Knowledge Graph Dashboard</div>', unsafe_allow_html=True)

    # Get statistics
    stats = neo4j_client.get_statistics()

    # Display metrics
    col1, col2, col3, col4 = st.columns(4)

    with col1:
        st.metric("Programs", stats['nodes'].get('CobolProgram', 0))

    with col2:
        st.metric("Data Files", stats['nodes'].get('DataFile', 0))

    with col3:
        st.metric("Procedures", stats['nodes'].get('Procedure', 0))

    with col4:
        st.metric("Relationships", stats['total_relationships'])

    # Recent processing results
    if st.session_state.processing_results:
        st.markdown("### Recent Processing Results")

        results_df = pd.DataFrame([
            {
                "Program": r['parsed_data'].get('program_name', 'N/A'),
                "Status": r['status'],
                "LOC": r['parsed_data'].get('loc', 0),
                "Complexity": r['enriched_data'].get('complexity_rating', 'N/A'),
                "Domain": r['enriched_data'].get('business_domain', 'N/A')
            }
            for r in st.session_state.processing_results[-10:]
        ])

        st.dataframe(results_df, use_container_width=True)


def show_upload_files():
    """Show file upload interface"""
    st.markdown('<div class="main-header">📁 Upload COBOL Files</div>', unsafe_allow_html=True)

    uploaded_files = st.file_uploader(
        "Upload COBOL files (.cob, .cbl, .cobol)",
        type=['cob', 'cbl', 'cobol'],
        accept_multiple_files=True
    )

    if uploaded_files and st.button("Process Files", type="primary"):
        st.markdown("### Processing Files...")

        progress_bar = st.progress(0)
        status_text = st.empty()
        results_container = st.container()

        total = len(uploaded_files)
        results = []

        for i, uploaded_file in enumerate(uploaded_files, 1):
            status_text.text(f"Processing {i}/{total}: {uploaded_file.name}")

            # Save temporary file
            with tempfile.NamedTemporaryFile(mode='wb', delete=False, suffix='.cob') as tmp:
                tmp.write(uploaded_file.getvalue())
                tmp_path = tmp.name

            try:
                # Process file
                result = orchestrator.process_file(tmp_path)
                results.append(result)

                # Display result
                with results_container:
                    if result['status'] == 'completed':
                        st.success(f"✅ {uploaded_file.name} - {result['parsed_data'].get('program_name', 'N/A')}")
                    else:
                        st.error(f"❌ {uploaded_file.name} - {result.get('errors', ['Unknown error'])}")

            except Exception as e:
                st.error(f"❌ {uploaded_file.name} - Error: {str(e)}")

            finally:
                # Clean up
                os.unlink(tmp_path)

            # Update progress
            progress_bar.progress(i / total)

        # Store results
        st.session_state.processing_results.extend(results)

        status_text.text(f"✅ Completed processing {total} files")
        st.balloons()


def show_clone_repository():
    """Show repository cloning interface"""
    st.markdown('<div class="main-header">🌐 Clone COBOL Repository</div>', unsafe_allow_html=True)

    st.markdown("""
    Clone a Git repository containing COBOL files and process all `.cob`, `.cbl`, and `.cobol` files.

    **Sample Repositories:**
    - https://github.com/openmainframeproject/cobol-programming-course
    - https://github.com/OCamlPro/gnucobol-contrib
    """)

    repo_url = st.text_input("Repository URL", placeholder="https://github.com/user/repo")

    col1, col2 = st.columns(2)
    with col1:
        max_files = st.number_input("Max files to process", min_value=1, max_value=1000, value=50)
    with col2:
        enable_enrichment = st.checkbox("Enable LLM Enrichment", value=True)

    if st.button("Clone and Process", type="primary", disabled=not repo_url):
        with st.spinner("Cloning repository..."):
            # Create temp directory
            temp_dir = tempfile.mkdtemp()

            try:
                # Clone repository
                Repo.clone_from(repo_url, temp_dir)
                st.success(f"✅ Repository cloned to {temp_dir}")

                # Find COBOL files
                cobol_files = []
                for ext in ['*.cob', '*.cbl', '*.cobol', '*.COB', '*.CBL', '*.COBOL']:
                    cobol_files.extend(Path(temp_dir).rglob(ext))

                cobol_files = [str(f) for f in cobol_files[:max_files]]

                if not cobol_files:
                    st.warning("No COBOL files found in repository")
                    return

                st.info(f"Found {len(cobol_files)} COBOL files")

                # Process files
                progress_bar = st.progress(0)
                status_text = st.empty()
                results_container = st.container()

                results = []

                for i, file_path in enumerate(cobol_files, 1):
                    status_text.text(f"Processing {i}/{len(cobol_files)}: {Path(file_path).name}")

                    try:
                        result = orchestrator.process_file(file_path)
                        results.append(result)

                        with results_container:
                            if result['status'] == 'completed':
                                program_name = result['parsed_data'].get('program_name', 'N/A')
                                st.success(f"✅ {Path(file_path).name} - {program_name}")
                            else:
                                st.error(f"❌ {Path(file_path).name}")

                    except Exception as e:
                        st.error(f"❌ {Path(file_path).name} - {str(e)}")

                    progress_bar.progress(i / len(cobol_files))

                # Store results
                st.session_state.processing_results.extend(results)

                status_text.text(f"✅ Completed! Processed {len(cobol_files)} files")

                # Show summary
                st.markdown("### Processing Summary")
                successful = sum(1 for r in results if r['status'] == 'completed')
                failed = len(results) - successful

                col1, col2, col3 = st.columns(3)
                col1.metric("Total Processed", len(results))
                col2.metric("Successful", successful)
                col3.metric("Failed", failed)

                st.balloons()

            except Exception as e:
                st.error(f"Error: {str(e)}")

            finally:
                # Cleanup
                shutil.rmtree(temp_dir, ignore_errors=True)


def show_query_graph():
    """Show query interface"""
    st.markdown('<div class="main-header">🔍 Query Knowledge Graph</div>', unsafe_allow_html=True)

    # Sample queries
    st.markdown("### Sample Queries")

    col1, col2 = st.columns(2)

    sample_queries = [
        "Show all programs with high complexity",
        "Which programs write to PRINT?",
        "List all programs in the Finance domain",
        "Show programs that call other programs",
        "Show me all programs with their complexity ratings"
    ]

    selected_sample = st.selectbox("Select a sample query", [""] + sample_queries)

    # Query input
    user_query = st.text_area(
        "Enter your question",
        value=selected_sample if selected_sample else "",
        placeholder="e.g., Which programs does CUSTMAST call?",
        height=100
    )

    if st.button("Execute Query", type="primary", disabled=not user_query):
        with st.spinner("Generating Cypher and executing query..."):
            try:
                result = orchestrator.query_graph(user_query)

                # Show cache indicator
                if result.get('cached'):
                    st.info("⚡ Results retrieved from cache")

                # Show natural language answer
                st.markdown("### Answer")
                answer = result.get('answer')
                if answer:
                    st.markdown(answer)
                else:
                    st.warning("Answer generation is in progress or unavailable. Please check the detailed results below.")

                # Show generated Cypher (collapsible)
                with st.expander("View Generated Cypher Query"):
                    st.code(result['generated_cypher'], language="cypher")

                # Show detailed results (collapsible)
                with st.expander("View Detailed Results"):
                    if result['query_results']:
                        df = pd.DataFrame(result['query_results'])
                        st.dataframe(df, use_container_width=True)
                        st.caption(f"Showing {len(result['query_results'])} results")
                    else:
                        st.info("No results found")

                # Store in history
                st.session_state.query_history.append({
                    "query": user_query,
                    "cypher": result['generated_cypher'],
                    "answer": answer,
                    "count": len(result['query_results'])
                })

            except Exception as e:
                st.error(f"Query failed: {str(e)}")

    # Query history
    if st.session_state.query_history:
        st.markdown("### Recent Queries")
        for i, query in enumerate(reversed(st.session_state.query_history[-5:]), 1):
            with st.expander(f"{i}. {query['query'][:50]}..."):
                st.markdown(query.get('answer', 'No answer'))
                st.code(query['cypher'], language="cypher")
                st.caption(f"Results: {query['count']}")


def show_analytics():
    """Show analytics and visualizations"""
    st.markdown('<div class="main-header">📈 Analytics</div>', unsafe_allow_html=True)

    # Complexity distribution
    st.markdown("### Complexity Distribution")

    complexity_query = """
    MATCH (p:CobolProgram)
    RETURN p.complexity as Complexity, count(*) as Count
    """

    try:
        results = neo4j_client.query(complexity_query)
        if results:
            df = pd.DataFrame(results)
            st.bar_chart(df.set_index('Complexity'))
    except:
        st.info("No data available")

    # Domain distribution
    st.markdown("### Business Domain Distribution")

    domain_query = """
    MATCH (p:CobolProgram)
    RETURN p.domain as Domain, count(*) as Count
    ORDER BY Count DESC
    """

    try:
        results = neo4j_client.query(domain_query)
        if results:
            df = pd.DataFrame(results)
            st.bar_chart(df.set_index('Domain'))
    except:
        st.info("No data available")

    # Top callers
    st.markdown("### Most Called Programs")

    top_called_query = """
    MATCH (caller:CobolProgram)-[:CALLS]->(called:CobolProgram)
    RETURN called.name as Program, count(*) as CallCount
    ORDER BY CallCount DESC
    LIMIT 10
    """

    try:
        results = neo4j_client.query(top_called_query)
        if results:
            df = pd.DataFrame(results)
            st.dataframe(df, use_container_width=True)
    except:
        st.info("No data available")


def show_documentation():
    """Show documentation generation page"""
    st.markdown('<p class="main-header">📄 Documentation Generator</p>', unsafe_allow_html=True)

    st.markdown("""
    Generate comprehensive documentation from your COBOL Knowledge Graph insights.
    Export documentation in various formats for team collaboration and reference.
    """)

    st.markdown("---")

    col1, col2 = st.columns([2, 1])

    with col1:
        st.markdown("### Document Configuration")

        # Document type selection
        doc_type = st.selectbox(
            "Document Type",
            options=["system_overview"],
            help="Select the type of document to generate"
        )

        # Format selection
        doc_format = st.selectbox(
            "Format",
            options=["markdown", "docx"],
            help="Select output format: Markdown (MD) or Word Document (DOCX)"
        )

        # Filters and limits
        st.markdown("### Filters & Scope")

        col_a, col_b = st.columns(2)

        with col_a:
            max_programs = st.number_input(
                "Max Programs to Document",
                min_value=1,
                max_value=50,
                value=10,
                help="Limit the number of programs to document (fewer = faster)"
            )

        with col_b:
            complexity_filter = st.selectbox(
                "Complexity Filter",
                ["All", "High", "Medium", "Low"],
                help="Filter by complexity level"
            )

        with st.expander("Advanced Filters", expanded=False):
            domain_filter = st.text_input("Domain", placeholder="e.g., Billing, Claims")

            st.info("💡 **Performance Tips:**\n"
                   "- Start with 5-10 programs to test\n"
                   "- Filter by 'High' complexity to document critical programs first\n"
                   "- Use domain filter to focus on specific business areas")

        # Generate button
        st.markdown("---")

        if st.button("🚀 Generate Document", type="primary", use_container_width=True):
            # Show progress container
            progress_bar = st.progress(0)
            status_text = st.empty()

            try:
                status_text.text("📊 Querying knowledge graph...")
                progress_bar.progress(10)

                # Prepare state
                from datetime import datetime

                filters = {}
                if domain_filter:
                    filters['domain'] = domain_filter
                if complexity_filter != "All":
                    filters['complexity'] = complexity_filter.lower()
                filters['max_programs'] = max_programs

                state = {
                    "doc_type": doc_type,
                    "format": doc_format,
                    "filters": filters,
                    "stage": "document_generation",
                    "status": "pending",
                    "errors": [],
                    "file_path": None,
                    "generation_time": 0.0,
                    "timestamp": datetime.utcnow().isoformat()
                }

                # Generate document
                status_text.text(f"🤖 Generating documentation for {max_programs} programs...")
                progress_bar.progress(30)

                start_time = time.time()
                result = document_generator_agent.process(state)
                generation_time = time.time() - start_time

                progress_bar.progress(100)
                status_text.text("✅ Documentation complete!")

                if result['status'] == 'completed':
                    st.success(f"✅ Document generated successfully in {generation_time:.2f}s")

                    # Show file path
                    file_path = result['file_path']
                    st.info(f"📁 File saved to: `{file_path}`")

                    # Download button
                    try:
                        # Handle different file formats
                        if file_path.endswith('.docx'):
                            # For DOCX, read as binary
                            with open(file_path, 'rb') as f:
                                file_content = f.read()

                            st.download_button(
                                label="📥 Download Word Document",
                                data=file_content,
                                file_name=Path(file_path).name,
                                mime="application/vnd.openxmlformats-officedocument.wordprocessingml.document",
                                use_container_width=True
                            )

                            st.info("📄 DOCX file generated successfully. Click the button above to download.")

                        else:
                            # For Markdown, read as text
                            with open(file_path, 'r', encoding='utf-8') as f:
                                file_content = f.read()

                            st.download_button(
                                label="📥 Download Markdown",
                                data=file_content,
                                file_name=Path(file_path).name,
                                mime="text/markdown",
                                use_container_width=True
                            )

                            # Preview for markdown
                            st.markdown("### Document Preview")
                            with st.expander("View Content", expanded=True):
                                st.markdown(file_content)

                    except Exception as e:
                        st.error(f"Error reading file: {e}")

                else:
                    st.error(f"❌ Document generation failed")
                    if result.get('errors'):
                        for error in result['errors']:
                            st.error(f"• {error}")

            except Exception as e:
                st.error(f"Error generating document: {e}")

    with col2:
        st.markdown("### Document Types")

        st.markdown("""
        **System Overview**
        - Executive summary
        - Program statistics
        - Domain breakdown
        - Complexity analysis
        - Top complex programs
        - File operations summary
        """)

        st.markdown("### Coming Soon")
        st.markdown("""
        - Program Detail Reports
        - Dependency Maps
        - Word/PDF Export
        """)


def show_modernization():
    """Show modernization analysis and recommendations"""
    st.markdown('<div class="main-header">🔧 Modernization Analysis</div>', unsafe_allow_html=True)

    st.markdown("""
    Analyze COBOL programs and receive AI-powered modernization recommendations based on:
    - **Risk Assessment**: Complexity, coupling, size, and data I/O
    - **Business Value**: Usage patterns, domain criticality, and integration impact
    - **Smart Strategies**: Rewrite, Strangler Fig, Retire/Replace, or Encapsulate
    """)

    # Configuration section
    st.markdown("### ⚙️ Analysis Configuration")

    col1, col2 = st.columns(2)

    with col1:
        max_programs = st.number_input(
            "Max Programs to Analyze",
            min_value=1,
            max_value=100,
            value=20,
            help="Limit the number of programs to analyze (fewer = faster)"
        )

    with col2:
        complexity_filter = st.selectbox(
            "Complexity Filter",
            ["All", "High", "Medium", "Low"],
            help="Filter by complexity level"
        )

    # Advanced filters
    with st.expander("🔍 Advanced Filters", expanded=False):
        domain_filter = st.text_input(
            "Domain Filter",
            placeholder="e.g., Billing, Financial, Customer",
            help="Filter by business domain (case-insensitive partial match)"
        )

        st.info("💡 **Tip**: Start with high-complexity or critical-domain programs for maximum impact.")

    # Build filters dictionary
    filters = {'max_programs': max_programs}
    if domain_filter:
        filters['domain'] = domain_filter
    if complexity_filter != "All":
        filters['complexity'] = complexity_filter.lower()

    # Analysis button
    if st.button("🚀 Run Modernization Analysis", type="primary", use_container_width=True):
        progress_bar = st.progress(0)
        status_text = st.empty()

        try:
            from datetime import datetime
            from utils.state import ModernizationState

            status_text.text("📊 Querying knowledge graph...")
            progress_bar.progress(20)

            # Create state
            state = ModernizationState(
                filters=filters,
                status='pending',
                errors=[],
                recommendations=[],
                analysis_time=0.0,
                timestamp=datetime.now().isoformat()
            )

            status_text.text(f"🤖 Analyzing {max_programs} programs with LLM...")
            progress_bar.progress(40)

            # Run analysis
            modernization_agent = ModernizationAgent()
            result = modernization_agent.process(state)

            progress_bar.progress(100)
            status_text.text("✅ Analysis complete!")

            if result['status'] == 'completed':
                recommendations = result['recommendations']

                if not recommendations:
                    st.warning("No programs found matching the specified filters.")
                    return

                st.success(f"✅ Analyzed {len(recommendations)} programs in {result['analysis_time']:.2f} seconds")

                # Store in session state
                st.session_state.modernization_results = recommendations

                # Summary metrics
                st.markdown("### 📊 Analysis Summary")

                col1, col2, col3, col4 = st.columns(4)

                with col1:
                    st.metric("Programs Analyzed", len(recommendations))

                with col2:
                    avg_risk = sum(r['risk_score'] for r in recommendations) / len(recommendations)
                    st.metric("Avg Risk Score", f"{avg_risk:.1f}/100")

                with col3:
                    avg_value = sum(r['value_score'] for r in recommendations) / len(recommendations)
                    st.metric("Avg Value Score", f"{avg_value:.1f}/100")

                with col4:
                    high_priority = sum(1 for r in recommendations if r['priority_score'] >= 70)
                    st.metric("High Priority", high_priority)

                # Strategy distribution
                st.markdown("### 📈 Recommended Strategies")

                strategy_counts = {}
                for rec in recommendations:
                    strategy = rec['strategy']
                    strategy_counts[strategy] = strategy_counts.get(strategy, 0) + 1

                strategy_df = pd.DataFrame([
                    {'Strategy': k, 'Count': v, 'Percentage': f"{(v/len(recommendations)*100):.1f}%"}
                    for k, v in strategy_counts.items()
                ])
                st.dataframe(strategy_df, use_container_width=True, hide_index=True)

            else:
                st.error(f"Analysis failed: {', '.join(result['errors'])}")

        except Exception as e:
            st.error(f"❌ Error during analysis: {str(e)}")
            progress_bar.empty()
            status_text.empty()

    # Display recommendations if available
    if 'modernization_results' in st.session_state and st.session_state.modernization_results:
        st.markdown("---")
        st.markdown("### 🎯 Detailed Recommendations")

        recommendations = st.session_state.modernization_results

        # Priority filter
        priority_filter = st.selectbox(
            "Filter by Priority",
            ["All", "High (70+)", "Medium (40-70)", "Low (<40)"],
            help="Filter recommendations by priority score"
        )

        filtered_recs = recommendations
        if priority_filter == "High (70+)":
            filtered_recs = [r for r in recommendations if r['priority_score'] >= 70]
        elif priority_filter == "Medium (40-70)":
            filtered_recs = [r for r in recommendations if 40 <= r['priority_score'] < 70]
        elif priority_filter == "Low (<40)":
            filtered_recs = [r for r in recommendations if r['priority_score'] < 40]

        if not filtered_recs:
            st.info("No recommendations match the selected priority filter.")
        else:
            # Display each recommendation in an expander
            for i, rec in enumerate(filtered_recs, 1):
                # Color-code by priority
                if rec['priority_score'] >= 70:
                    priority_color = "🔴"
                elif rec['priority_score'] >= 40:
                    priority_color = "🟡"
                else:
                    priority_color = "🟢"

                with st.expander(
                    f"{priority_color} **{rec['program_name']}** | "
                    f"Priority: {rec['priority_score']:.1f} | "
                    f"Strategy: {rec['strategy']}",
                    expanded=(i <= 3)  # Expand first 3
                ):
                    # Header info
                    col1, col2, col3 = st.columns(3)
                    with col1:
                        st.metric("Risk Score", f"{rec['risk_score']:.1f}/100")
                    with col2:
                        st.metric("Value Score", f"{rec['value_score']:.1f}/100")
                    with col3:
                        st.metric("Priority Score", f"{rec['priority_score']:.1f}/100")

                    # Program details
                    st.markdown("**Program Details:**")
                    st.markdown(f"- **Domain**: {rec['domain']}")
                    st.markdown(f"- **Complexity**: {rec['complexity']}")
                    st.markdown(f"- **Lines of Code**: {int(rec['loc'])}")

                    # Risk factors
                    st.markdown("**🚨 Risk Factors:**")
                    for factor in rec['risk_factors']:
                        st.markdown(f"- {factor}")

                    # Value factors
                    st.markdown("**💎 Value Factors:**")
                    for factor in rec['value_factors']:
                        st.markdown(f"- {factor}")

                    # Strategy
                    st.markdown(f"**📋 Recommended Strategy: `{rec['strategy']}`**")
                    st.markdown(rec['recommended_approach'])

                    # Technologies
                    st.markdown("**🛠️ Technology Recommendations:**")
                    for tech in rec['technology_recommendations']:
                        st.markdown(f"- {tech}")

                    # Effort estimate
                    st.markdown(f"**⏱️ Estimated Effort:** {rec['estimated_effort']}")

                    # Key considerations
                    st.markdown("**⚠️ Key Considerations:**")
                    for consideration in rec['key_considerations']:
                        st.markdown(f"- {consideration}")

            # Export recommendations
            st.markdown("---")
            st.markdown("### 📥 Export Recommendations")

            # Create CSV export
            export_data = []
            for rec in filtered_recs:
                export_data.append({
                    'Program': rec['program_name'],
                    'Domain': rec['domain'],
                    'Complexity': rec['complexity'],
                    'LOC': int(rec['loc']),
                    'Risk Score': round(rec['risk_score'], 1),
                    'Value Score': round(rec['value_score'], 1),
                    'Priority Score': round(rec['priority_score'], 1),
                    'Strategy': rec['strategy'],
                    'Estimated Effort': rec['estimated_effort']
                })

            export_df = pd.DataFrame(export_data)

            st.download_button(
                label="📥 Download as CSV",
                data=export_df.to_csv(index=False),
                file_name=f"modernization_recommendations_{datetime.now().strftime('%Y%m%d_%H%M%S')}.csv",
                mime="text/csv",
                use_container_width=True
            )


def show_translation():
    """Show code translation page"""
    st.markdown('<div class="main-header">🔄 Code Translation</div>', unsafe_allow_html=True)

    st.markdown("""
    Translate COBOL programs to modern languages using AI-powered translation with rich business context from the Knowledge Graph.

    **Supported Languages**: Java (Spring Boot), Python (FastAPI), C# (.NET Core)
    """)

    # Get all programs from KG
    try:
        query = """
        MATCH (p:CobolProgram)
        RETURN p.name AS name,
               COALESCE(p.domain, 'Unknown') AS domain,
               COALESCE(p.complexity_score, 0) AS complexity,
               COALESCE(p.loc, 0) AS loc
        ORDER BY p.name
        """
        programs_data = neo4j_client.query(query)

        if not programs_data:
            st.warning("⚠️ No programs found in knowledge graph. Please upload and process COBOL files first.")
            return

        program_options = [f"{p['name']} ({p['domain']}, {int(p['loc'])} LOC)" for p in programs_data]
        program_names = [p['name'] for p in programs_data]

    except Exception as e:
        st.error(f"Error loading programs: {e}")
        return

    # Program Selection
    st.markdown("### 📋 Select Programs to Translate")

    selected_indices = st.multiselect(
        "Programs",
        options=list(range(len(program_options))),
        format_func=lambda i: program_options[i],
        help="Select one or more programs to translate"
    )

    if not selected_indices:
        st.info("👆 Select at least one program to begin translation")
        return

    selected_programs = [program_names[i] for i in selected_indices]

    st.success(f"✅ Selected {len(selected_programs)} program(s): {', '.join(selected_programs)}")

    # Translation Configuration
    st.markdown("### ⚙️ Translation Configuration")

    col1, col2 = st.columns(2)

    with col1:
        target_language = st.selectbox(
            "Target Language",
            options=["java", "python", "csharp"],
            format_func=lambda x: {"java": "☕ Java", "python": "🐍 Python", "csharp": "💎 C#"}[x],
            help="Select the target programming language"
        )

    with col2:
        # Get available frameworks for selected language
        frameworks = LANGUAGE_CONFIG[target_language]['frameworks']
        framework_display = {
            'spring-boot': 'Spring Boot (Recommended)',
            'fastapi': 'FastAPI (Recommended)',
            'dotnet-core': '.NET Core (Recommended)',
            'plain-java': 'Plain Java',
            'plain-python': 'Plain Python',
            'flask': 'Flask',
            'plain-csharp': 'Plain C#'
        }

        target_framework = st.selectbox(
            "Framework",
            options=frameworks,
            format_func=lambda x: framework_display.get(x, x),
            help="Select the target framework"
        )

    # Advanced Options
    with st.expander("🔧 Advanced Options", expanded=False):
        include_tests = st.checkbox(
            "Generate Test Stubs",
            value=True,
            help="Generate basic test structure with TODOs"
        )

        include_comments = st.checkbox(
            "Preserve Comments & Add Documentation",
            value=True,
            help="Add detailed comments explaining COBOL origins"
        )

        package_name = st.text_input(
            "Package/Namespace (Optional)",
            placeholder="com.example.legacy" if target_language == "java" else "MyApp.Legacy" if target_language == "csharp" else "",
            help="Optional package or namespace for the generated code"
        )

    # Translation Info
    st.info(f"""
💡 **Translation Details:**
- {len(selected_programs)} program(s) will be translated to {target_language.upper()} using {target_framework}
- Output will be saved to `exports/translations/{target_language}/{{program_name}}/`
- Each program gets: Main code file, Test stub, README with conversion notes
    """)

    # Translate Button
    if st.button("🚀 Translate Selected Programs", type="primary", use_container_width=True):
        progress_bar = st.progress(0)
        status_text = st.empty()

        try:
            from datetime import datetime as dt
            from utils.state import CodeTranslationState

            status_text.text("🔄 Initializing translation...")
            progress_bar.progress(10)

            # Create state
            state = CodeTranslationState(
                program_names=selected_programs,
                target_language=target_language,
                target_framework=target_framework,
                include_tests=include_tests,
                include_comments=include_comments,
                package_name=package_name if package_name else None,
                status='pending',
                errors=[],
                translations=[],
                translation_time=0.0,
                timestamp=dt.now().isoformat()
            )

            status_text.text(f"🤖 Translating {len(selected_programs)} program(s) with LLM...")
            progress_bar.progress(30)

            # Run translation
            result = code_translation_agent.process(state)

            progress_bar.progress(100)
            status_text.text("✅ Translation complete!")

            if result['status'] == 'completed':
                translations = result['translations']
                success_count = sum(1 for t in translations if t['translation_success'])

                st.success(f"✅ Successfully translated {success_count}/{len(translations)} program(s) in {result['translation_time']:.2f} seconds")

                # Display results
                st.markdown("---")
                st.markdown("### 📦 Translation Results")

                for translation in translations:
                    if not translation['translation_success']:
                        with st.expander(f"❌ {translation['program_name']} - Translation Failed", expanded=False):
                            st.error(f"Error: {translation.get('error_message', 'Unknown error')}")
                        continue

                    with st.expander(f"✅ {translation['program_name']} - Translation Successful", expanded=True):
                        # Show file paths
                        st.markdown("**Generated Files:**")
                        st.code(f"""
Main Code: {translation['main_file_path']}
Test Stub: {translation.get('test_file_path', 'N/A')}
README:    {translation['readme_path']}
                        """)

                        # Conversion Notes
                        if translation['conversion_notes']:
                            st.markdown("**🔄 Conversion Notes:**")
                            for note in translation['conversion_notes']:
                                st.markdown(f"- {note}")

                        # Manual Review Items
                        if translation['manual_review_items']:
                            st.markdown("**⚠️ Manual Review Required:**")
                            for item in translation['manual_review_items']:
                                st.warning(item)

                        # Code Preview Tabs
                        tab1, tab2, tab3 = st.tabs(["📄 Main Code", "🧪 Test Stub", "📖 README"])

                        with tab1:
                            st.code(translation['main_code'], language=target_language)
                            st.download_button(
                                "📥 Download Main Code",
                                translation['main_code'],
                                file_name=Path(translation['main_file_path']).name,
                                mime="text/plain"
                            )

                        with tab2:
                            if translation.get('test_code'):
                                st.code(translation['test_code'], language=target_language)
                                st.download_button(
                                    "📥 Download Test Code",
                                    translation['test_code'],
                                    file_name=Path(translation['test_file_path']).name,
                                    mime="text/plain"
                                )
                            else:
                                st.info("Test stub generation was disabled")

                        with tab3:
                            # Read README from file
                            try:
                                with open(translation['readme_path'], 'r', encoding='utf-8') as f:
                                    readme_content = f.read()
                                st.markdown(readme_content)
                                st.download_button(
                                    "📥 Download README",
                                    readme_content,
                                    file_name="README.md",
                                    mime="text/markdown"
                                )
                            except Exception as e:
                                st.error(f"Could not load README: {e}")

                # Provide zip download for all files
                st.markdown("---")
                st.markdown("### 📦 Download All Translations")

                import zipfile
                import io

                zip_buffer = io.BytesIO()
                with zipfile.ZipFile(zip_buffer, 'w', zipfile.ZIP_DEFLATED) as zip_file:
                    for translation in translations:
                        if translation['translation_success']:
                            # Add main code
                            zip_file.write(
                                translation['main_file_path'],
                                arcname=f"{translation['program_name']}/{Path(translation['main_file_path']).name}"
                            )
                            # Add test if exists
                            if translation.get('test_file_path'):
                                zip_file.write(
                                    translation['test_file_path'],
                                    arcname=f"{translation['program_name']}/{Path(translation['test_file_path']).name}"
                                )
                            # Add README
                            zip_file.write(
                                translation['readme_path'],
                                arcname=f"{translation['program_name']}/README.md"
                            )

                zip_buffer.seek(0)
                st.download_button(
                    label="📦 Download All as ZIP",
                    data=zip_buffer,
                    file_name=f"cobol_translations_{target_language}_{dt.now().strftime('%Y%m%d_%H%M%S')}.zip",
                    mime="application/zip",
                    use_container_width=True
                )

            else:
                st.error(f"Translation failed: {', '.join(result['errors'])}")

        except Exception as e:
            st.error(f"❌ Error during translation: {str(e)}")
            import traceback
            st.code(traceback.format_exc())
            progress_bar.empty()
            status_text.empty()


def main():
    """Main application"""
    init_session_state()
    page = show_sidebar()

    if page == "📊 Dashboard":
        show_dashboard()
    elif page == "📁 Upload Files":
        show_upload_files()
    elif page == "🌐 Clone Repository":
        show_clone_repository()
    elif page == "🔍 Query Graph":
        show_query_graph()
    elif page == "📈 Analytics":
        show_analytics()
    elif page == "📄 Documentation":
        show_documentation()
    elif page == "🔧 Modernization":
        show_modernization()
    elif page == "🔄 Translation":
        show_translation()


if __name__ == "__main__":
    main()
