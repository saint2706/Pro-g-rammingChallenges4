# Chatbot – FastAPI Microservice

This variant wraps the reference rule-based chatbot in a small FastAPI
application. It keeps the conversation state on disk between requests using the
helpers from the canonical Python solution.

## Running locally

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
uvicorn app:app --reload
```

Then send chat messages:

```bash
curl -X POST http://127.0.0.1:8000/chat \
     -H "Content-Type: application/json" \
     -d '{"message": "hello"}'
```

The resulting JSON includes the updated conversation history. A simple health
check is exposed at `GET /health`.
