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

    # LLM Provider Configuration
    llm_provider: str = "openai"  # Options: "openai", "groq", or "google"

    # OpenAI Configuration
    openai_api_key: str = ""
    openai_model: str = "gpt-4o-mini"

    # Groq Configuration
    groq_api_key: str = ""
    groq_model: str = "llama-3.1-8b-instant"  # Options: "llama-3.1-8b-instant", "llama-3.3-70b-versatile", "qwen/qwen3-32b"

    # Google Gemini Configuration
    google_api_key: str = ""
    gemini_model: str = "gemini-2.5-flash"  # Options: "gemini-2.0-flash-exp", "gemini-2.5-flash", "gemini-3-pro-preview"

    # Shared LLM Configuration
    llm_temperature: float = 0.0
    llm_max_tokens: int = 500

    # Neo4j Configuration
    neo4j_uri: str = "bolt://localhost:7687"
    neo4j_username: str = "neo4j"
    neo4j_password: str = ""

    # LangSmith Configuration (for tracing and evals)
    langchain_tracing_v2: bool = False
    langsmith_api_key: str = ""
    langchain_project: str = "cobol-insights"
    langchain_endpoint: str = "https://api.smith.langchain.com"

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

        # Set Groq API key in environment
        if self.groq_api_key:
            os.environ["GROQ_API_KEY"] = self.groq_api_key

        # Set Google API key in environment
        if self.google_api_key:
            os.environ["GOOGLE_API_KEY"] = self.google_api_key

        # Set LangSmith environment variables for tracing and evals
        if self.langsmith_api_key:
            os.environ["LANGCHAIN_TRACING_V2"] = str(self.langchain_tracing_v2).lower()
            os.environ["LANGSMITH_API_KEY"] = self.langsmith_api_key
            os.environ["LANGCHAIN_PROJECT"] = self.langchain_project
            os.environ["LANGCHAIN_ENDPOINT"] = self.langchain_endpoint

    def get_llm_model(self):
        """Get the current LLM model name based on provider"""
        if self.llm_provider == "groq":
            return self.groq_model
        elif self.llm_provider == "google":
            return self.gemini_model
        return self.openai_model

    def get_llm_api_key(self):
        """Get the current LLM API key based on provider"""
        if self.llm_provider == "groq":
            return self.groq_api_key
        elif self.llm_provider == "google":
            return self.google_api_key
        return self.openai_api_key


# Global settings instance
settings = Settings()


def validate_settings():
    """Validate required settings are present"""
    errors = []

    # Check if at least one LLM provider is configured
    if settings.llm_provider == "openai" and not settings.openai_api_key:
        errors.append("OPENAI_API_KEY is required when LLM_PROVIDER is 'openai'")
    elif settings.llm_provider == "groq" and not settings.groq_api_key:
        errors.append("GROQ_API_KEY is required when LLM_PROVIDER is 'groq'")
    elif settings.llm_provider == "google" and not settings.google_api_key:
        errors.append("GOOGLE_API_KEY is required when LLM_PROVIDER is 'google'")

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
