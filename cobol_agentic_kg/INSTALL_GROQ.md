# Groq Integration - Installation & Usage Guide

## ✅ All Changes Complete!

Groq support has been fully integrated into the COBOL Insights application.

## 📦 Installation Steps

### 1. Install Groq Package

```bash
cd cobol_agentic_kg
pip install langchain-groq
```

Or install all requirements:
```bash
pip install -r requirements.txt
```

### 2. Verify Configuration

Your `.env` file should be configured with:
```env
LLM_PROVIDER=groq
GROQ_API_KEY=your_groq_api_key_here
GROQ_MODEL=llama-3.1-8b-instant
```

### 3. Test the Setup

```bash
python test_groq_setup.py
```

You should see:
```
✅ LLM instance created successfully
Response: Hello from COBOL Insights!
✅ ALL TESTS PASSED
```

## 🚀 Running the Streamlit UI

### Start the UI:

```bash
cd cobol_agentic_kg
streamlit run ui/app.py
```

### In the UI Sidebar:

You'll see **🤖 LLM Configuration** section:
- **Provider**: Select `groq` or `openai`
- **Model**: Choose from available models
  - Groq: `llama-3.1-8b-instant`, `llama-3.3-70b-versatile`, etc.
  - OpenAI: `gpt-4o-mini`, `gpt-4o`, etc.

The selection updates instantly - no restart needed!

## 🔄 Switching Providers

### Option 1: Via UI (Recommended)
1. Open Streamlit app
2. Use sidebar dropdowns to switch provider/model
3. Changes apply immediately

### Option 2: Via .env File
Edit `.env`:
```env
# Use Groq (FREE)
LLM_PROVIDER=groq
GROQ_MODEL=llama-3.1-8b-instant

# Or use OpenAI
LLM_PROVIDER=openai
OPENAI_MODEL=gpt-4o-mini
```

Then restart the app.

## 📋 Available Models

### Groq Models (FREE)
- ⚡ **llama-3.1-8b-instant** - Fastest, great for development
- 🚀 **llama-3.3-70b-versatile** - More powerful
- 🔬 **qwen/qwen-2.5-72b-instruct** - Alternative large model
- 📝 **mixtral-8x7b-32768** - Long context (32K tokens)

### OpenAI Models (Paid)
- 💎 **gpt-4o** - Most capable
- ⚡ **gpt-4o-mini** - Fast and affordable
- 🔧 **gpt-4-turbo** - Balanced performance

## 💰 Cost Comparison

| Provider | Model | Cost | Speed |
|----------|-------|------|-------|
| Groq | llama-3.1-8b-instant | **FREE** | ~800 tokens/sec |
| Groq | llama-3.3-70b-versatile | **FREE** | ~400 tokens/sec |
| OpenAI | gpt-4o-mini | $0.15/1M tokens | ~100 tokens/sec |
| OpenAI | gpt-4o | $2.50/1M tokens | ~80 tokens/sec |

**Recommendation**: Use Groq for development/testing, OpenAI for production if you need GPT-4 quality.

## 🧪 Testing Different Providers

### Test Cypher Generation:

```python
# In Python console
from utils.llm_factory import set_llm_provider, get_llm

# Test Groq
set_llm_provider("groq", "llama-3.1-8b-instant")
llm = get_llm()
response = llm.invoke("Generate Cypher: Show all programs")
print(response.content)

# Test OpenAI
set_llm_provider("openai", "gpt-4o-mini")
llm = get_llm()
response = llm.invoke("Generate Cypher: Show all programs")
print(response.content)
```

## 🎯 What's Been Updated

### Files Modified:
1. ✅ `config/settings.py` - Added LLM provider config
2. ✅ `utils/llm_factory.py` - NEW - LLM factory for multi-provider support
3. ✅ `agents/cypher_gen.py` - Uses llm_factory
4. ✅ `agents/enrichment.py` - Uses llm_factory
5. ✅ `agents/retrieval.py` - Uses llm_factory
6. ✅ `ui/app.py` - Added provider/model selector in sidebar
7. ✅ `.env` - Updated with Groq config
8. ✅ `requirements.txt` - Added langchain-groq

### New Features:
- ✨ Switch LLM providers via UI
- ✨ Choose specific models per provider
- ✨ Cost indicator (FREE vs Paid)
- ✨ Real-time provider switching (no restart needed)

## 🐛 Troubleshooting

### Error: "No module named 'langchain_groq'"
```bash
pip install langchain-groq
```

### Error: "GROQ_API_KEY is required"
1. Check `.env` file has `GROQ_API_KEY=...`
2. Verify API key from https://console.groq.com

### UI doesn't show LLM selector
1. Restart Streamlit: `streamlit run ui/app.py`
2. Clear browser cache (Ctrl+Shift+R)

### Groq API rate limits
- Free tier: ~30 requests/minute
- If you hit limits, switch to OpenAI temporarily

## 📚 Resources

- Groq Console: https://console.groq.com
- Groq Docs: https://console.groq.com/docs
- LangChain Groq: https://python.langchain.com/docs/integrations/chat/groq

## ✅ Ready to Go!

Everything is configured and ready. Run:

```bash
streamlit run ui/app.py
```

And start using Groq for faster, free LLM inference! 🚀
