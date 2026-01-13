"""
LLM Factory - Creates LLM instances based on provider configuration
"""
from langchain_openai import ChatOpenAI
from langchain_groq import ChatGroq
from langchain_google_genai import ChatGoogleGenerativeAI
from config.settings import settings
from utils.logger import logger


def get_llm(temperature: float = None, max_tokens: int = None, model: str = None):
    """
    Get LLM instance based on configured provider

    Args:
        temperature: Override default temperature
        max_tokens: Override default max_tokens
        model: Override default model name

    Returns:
        LLM instance (ChatOpenAI, ChatGroq, or ChatGoogleGenerativeAI)
    """
    provider = settings.llm_provider.lower()

    if provider == "google":
        model_name = model or settings.gemini_model
        temp = temperature if temperature is not None else settings.llm_temperature
        tokens = max_tokens if max_tokens is not None else settings.llm_max_tokens

        logger.info(f"🌟 Using Google Gemini model: {model_name}")

        return ChatGoogleGenerativeAI(
            model=model_name,
            temperature=temp,
            max_output_tokens=tokens,
            google_api_key=settings.google_api_key
        )

    elif provider == "groq":
        model_name = model or settings.groq_model
        temp = temperature if temperature is not None else settings.llm_temperature
        tokens = max_tokens if max_tokens is not None else settings.llm_max_tokens

        logger.info(f"🚀 Using Groq model: {model_name}")

        return ChatGroq(
            model=model_name,
            temperature=temp,
            max_tokens=tokens,
            groq_api_key=settings.groq_api_key
        )

    elif provider == "openai":
        model_name = model or settings.openai_model
        logger.info(f"🤖 Using OpenAI model: {model_name}")
        logger.info(f"📊 Input params - temperature: {temperature}, max_tokens: {max_tokens}")

        # O1 series models and GPT-5 don't support custom temperature
        # They only work with temperature=1 (default)
        reasoning_models = ["gpt-5", "gpt-4.5", "o1-preview", "o1-mini", "o1"]
        is_reasoning_model = any(model_name.lower().startswith(rm) or rm in model_name.lower() for rm in reasoning_models)

        logger.info(f"🔍 Model check - is_reasoning_model: {is_reasoning_model}")
        logger.info(f"🔍 Model name lowercase: '{model_name.lower()}'")
        logger.info(f"🔍 Checking against: {reasoning_models}")

        if is_reasoning_model:
            logger.info(f"⚠️  Reasoning model detected - using temperature=1 (required)")
            # Reasoning models require temperature=1 (not 0, not default 0.7)
            return ChatOpenAI(
                model=model_name,
                temperature=1,
                api_key=settings.openai_api_key
            )
        else:
            temp = temperature if temperature is not None else settings.llm_temperature
            tokens = max_tokens if max_tokens is not None else settings.llm_max_tokens

            logger.info(f"📊 Regular model - using temperature: {temp}, max_tokens: {tokens}")

            return ChatOpenAI(
                model=model_name,
                temperature=temp,
                max_tokens=tokens,
                api_key=settings.openai_api_key
            )

    else:
        raise ValueError(f"Unsupported LLM provider: {provider}. Use 'openai', 'groq', or 'google'")


def get_available_models():
    """
    Get list of available models for current provider

    Returns:
        Dict with provider name and list of available models
    """
    provider = settings.llm_provider.lower()

    if provider == "google":
        return {
            "provider": "google",
            "models": [
                "gemini-2.0-flash-exp",
                "gemini-2.5-flash",
                "gemini-3-pro-preview"
            ]
        }
    elif provider == "groq":
        return {
            "provider": "groq",
            "models": [
                "llama-3.1-8b-instant",
                "llama-3.3-70b-versatile",
                "llama-3.1-70b-versatile",
                "mixtral-8x7b-32768",
                "qwen/qwen3-32b"
            ]
        }
    elif provider == "openai":
        return {
            "provider": "openai",
            "models": [
                "gpt-5",
                "gpt-4o",
                "gpt-4o-mini",
                "gpt-4-turbo",
                "gpt-3.5-turbo"
            ]
        }
    else:
        return {"provider": "unknown", "models": []}


def set_llm_provider(provider: str, model: str = None):
    """
    Set the LLM provider and optionally the model

    Args:
        provider: "openai", "groq", or "google"
        model: Optional model name
    """
    if provider not in ["openai", "groq", "google"]:
        raise ValueError(f"Invalid provider: {provider}. Use 'openai', 'groq', or 'google'")

    settings.llm_provider = provider

    if model:
        if provider == "groq":
            settings.groq_model = model
        elif provider == "openai":
            settings.openai_model = model
        elif provider == "google":
            settings.gemini_model = model

    logger.info(f"✅ LLM provider set to: {provider}" + (f" with model: {model}" if model else ""))
