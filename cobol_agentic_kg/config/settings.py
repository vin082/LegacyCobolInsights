"""
Configuration settings for COBOL Agentic KG System
"""
import os
from pathlib import Path
from pydantic_settings import BaseSettings
from typing import Optional
from dotenv import load_dotenv

# Load .env file explicitly (for Streamlit compatibility)
env_path = Path(__file__).parent.parent / ".env"
load_dotenv(env_path)


class Settings(BaseSettings):
    """Application settings loaded from environment variables"""

    # OpenAI Configuration
    openai_api_key: str = ""
    llm_model: str = "gpt-4o-mini"
    llm_temperature: float = 0.0
    llm_max_tokens: int = 500

    # Neo4j Configuration
    neo4j_uri: str = "bolt://localhost:7687"
    neo4j_username: str = "neo4j"
    neo4j_password: str = ""

    # Processing Configuration
    batch_size: int = 100
    max_workers: int = 10
    enable_llm_enrichment: bool = True

    # Cache Configuration
    cache_enabled: bool = True
    redis_enabled: bool = False  # Set to True if Redis is available
    redis_url: str = "redis://localhost:6379/0"
    cache_ttl: int = 3600  # Cache time-to-live in seconds (1 hour)

    # Application Settings
    log_level: str = "INFO"

    # Streamlit Configuration
    app_title: str = "COBOL Knowledge Graph System"
    app_icon: str = "🕸️"
    theme: str = "light"

    class Config:
        env_file = ".env"
        env_file_encoding = "utf-8"
        case_sensitive = False

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        # Set OpenAI API key in environment for langchain
        if self.openai_api_key:
            os.environ["OPENAI_API_KEY"] = self.openai_api_key


# Global settings instance
settings = Settings()


def validate_settings():
    """Validate required settings are present"""
    errors = []

    if not settings.openai_api_key:
        errors.append("OPENAI_API_KEY is required")

    if not settings.neo4j_password:
        errors.append("NEO4J_PASSWORD is required")

    if errors:
        raise ValueError(f"Configuration errors: {', '.join(errors)}")

    return True


# Validate on import
try:
    validate_settings()
except ValueError as e:
    print(f"⚠️  Warning: {e}")
    print("Please set required environment variables in .env file")
