"""
Neo4j connection manager with connection pooling
"""
from langchain_neo4j import Neo4jGraph
from typing import Optional, Dict, Any, List
from config.settings import settings
import logging

logger = logging.getLogger(__name__)


class Neo4jClient:
    """
    Singleton Neo4j client with connection pooling and error handling
    """

    _instance: Optional['Neo4jClient'] = None
    _graph: Optional[Neo4jGraph] = None

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance

    def __init__(self):
        """Initialize Neo4j connection"""
        if self._graph is None:
            self.connect()

    def connect(self):
        """Establish connection to Neo4j"""
        try:
            self._graph = Neo4jGraph(
                url=settings.neo4j_uri,
                username=settings.neo4j_username,
                password=settings.neo4j_password
            )
            logger.info(f"✓ Connected to Neo4j at {settings.neo4j_uri}")
        except Exception as e:
            logger.error(f"Failed to connect to Neo4j: {e}")
            raise

    @property
    def graph(self) -> Neo4jGraph:
        """Get the Neo4j graph instance"""
        if self._graph is None:
            self.connect()
        return self._graph

    def query(self, cypher: str, params: Optional[Dict[str, Any]] = None) -> List[Dict[str, Any]]:
        """
        Execute a Cypher query

        Args:
            cypher: Cypher query string
            params: Query parameters

        Returns:
            Query results as list of dictionaries
        """
        try:
            return self.graph.query(cypher, params or {})
        except Exception as e:
            logger.error(f"Query execution failed: {e}")
            logger.error(f"Cypher: {cypher}")
            logger.error(f"Params: {params}")
            raise

    def clear_cobol_data(self):
        """Clear all COBOL-related nodes from the graph"""
        query = """
        MATCH (n)
        WHERE n:CobolProgram OR n:Cobolprogram OR
              n:Procedure OR n:DataFile OR n:Datafile OR
              n:Variable
        DETACH DELETE n
        """
        try:
            self.query(query)
            logger.info("✓ Cleared COBOL data from graph")
        except Exception as e:
            logger.error(f"Failed to clear COBOL data: {e}")
            raise

    def get_statistics(self) -> Dict[str, Any]:
        """
        Get graph statistics

        Returns:
            Dictionary with node counts, relationship counts, etc.
        """
        stats_query = """
        MATCH (n)
        WHERE n:CobolProgram OR n:Cobolprogram OR
              n:Procedure OR n:DataFile OR n:Datafile OR
              n:Variable
        RETURN labels(n)[0] AS label, count(*) AS count
        """

        rel_stats_query = """
        MATCH ()-[r]->()
        WHERE type(r) IN ['CALLS', 'READS', 'WRITES', 'CONTAINS_PROCEDURE', 'DECLARES_VARIABLE']
        RETURN type(r) AS relationship, count(*) AS count
        """

        try:
            node_stats = self.query(stats_query)
            rel_stats = self.query(rel_stats_query)

            return {
                "nodes": {row['label']: row['count'] for row in node_stats},
                "relationships": {row['relationship']: row['count'] for row in rel_stats},
                "total_nodes": sum(row['count'] for row in node_stats),
                "total_relationships": sum(row['count'] for row in rel_stats)
            }
        except Exception as e:
            logger.error(f"Failed to get statistics: {e}")
            return {"nodes": {}, "relationships": {}, "total_nodes": 0, "total_relationships": 0}

    def create_indexes(self):
        """Create indexes for better query performance"""
        indexes = [
            "CREATE INDEX cobol_program_name IF NOT EXISTS FOR (p:CobolProgram) ON (p.name)",
            "CREATE INDEX datafile_name IF NOT EXISTS FOR (f:DataFile) ON (f.name)",
            "CREATE INDEX procedure_name IF NOT EXISTS FOR (p:Procedure) ON (p.id)",
        ]

        for index_query in indexes:
            try:
                self.query(index_query)
                logger.info(f"Created index: {index_query}")
            except Exception as e:
                logger.warning(f"Index creation failed (may already exist): {e}")

    def health_check(self) -> bool:
        """
        Check if Neo4j connection is healthy

        Returns:
            True if connection is healthy, False otherwise
        """
        try:
            result = self.query("RETURN 1 as test")
            return len(result) > 0 and result[0]['test'] == 1
        except:
            return False


# Global client instance
neo4j_client = Neo4jClient()
