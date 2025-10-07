# Chatbot Challenge

This module implements the Practical challenge "Chatbot (with conversation retention)".

## Features

* **Rule-based responses** – Keyword and token matching with deterministic seeding.
* **Persistent memory** – Conversations are stored in JSON (`~/.simple_chatbot_history.json` by default) so you can continue across sessions.
* **Configurable history length** – Limit how many exchanges remain in memory with `--memory-length`.
* **History management** – Import from or export to any JSON file with `--import-history` / `--export-history`.
* **CLI quality-of-life** – Colored prompts, single-turn mode, rule dumping, deterministic randomness, and custom exit phrases.

## Conversation Retention

History is represented as a list of `{"user": ..., "bot": ...}` entries. The CLI keeps the history file updated after every exchange. If you would like to start from a saved transcript:

```bash
python chatbot.py --import-history backup.json
```

To back up your current conversation without chatting, export it:

```bash
python chatbot.py --export-history backup.json
```

The same JSON structure is used internally, making it easy to version or inspect the transcript manually.

## Alternative Implementations

The `solutions/` directory collects additional takes on the challenge using
different technology stacks:

* `python_fastapi/` – REST microservice exposing `/chat` and `/health` endpoints
  via FastAPI and Uvicorn.
* `node_cli/` – Interactive Node.js terminal chatbot that mirrors the Python
  rule matching logic and keeps history in `history.json`.

Each subdirectory contains its own setup instructions and optional dependency
lists so you can explore the same core idea in a different environment.

## Testing

Run the automated tests (requires `pytest`) to verify persistence behaviour:

```bash
pytest Practical/Chatbot/tests
```
