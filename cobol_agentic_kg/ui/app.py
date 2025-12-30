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
            ["📊 Dashboard", "📁 Upload Files", "🌐 Clone Repository", "🔍 Query Graph", "📈 Analytics"],
            label_visibility="collapsed"
        )

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


if __name__ == "__main__":
    main()
