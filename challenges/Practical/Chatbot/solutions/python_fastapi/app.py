"""FastAPI wrapper around the rule-based chatbot implementation.

Run with:
    uvicorn app:app --reload

This service exposes a `/chat` endpoint that keeps per-process history using the
same persistence helpers from the reference Python CLI solution.
"""

from __future__ import annotations

import random
import sys
from pathlib import Path
from typing import List, Dict

from fastapi import FastAPI, HTTPException
from pydantic import BaseModel

# Re-use the existing rule-based chatbot implementation by importing the module
# located in the parent directory. We add the parent folder to sys.path so the
# module can be discovered regardless of the working directory.
CURRENT_DIR = Path(__file__).resolve().parent
CHATBOT_DIR = CURRENT_DIR.parent
if str(CHATBOT_DIR) not in sys.path:
    sys.path.insert(0, str(CHATBOT_DIR))

try:
    import chatbot  # type: ignore
except ModuleNotFoundError as exc:  # pragma: no cover - defensive
    raise RuntimeError("Unable to import base chatbot implementation") from exc

RuleSet = chatbot.RuleSet
SimpleChatbot = chatbot.SimpleChatbot
load_history = chatbot.load_history
save_history = chatbot.save_history
DEFAULT_RULES: Dict[str, List[str]] = chatbot.DEFAULT_RULES


def _build_bot(history_path: Path) -> SimpleChatbot:
    rules = RuleSet(responses={k: list(v) for k, v in DEFAULT_RULES.items()})
    rules.ensure_default()
    history = []
    if history_path.exists():
        try:
            history = load_history(history_path)
        except Exception:  # pragma: no cover - history loading is best-effort
            history = []
    # Deterministic behaviour between restarts for reproducible demos.
    random.seed(42)
    return SimpleChatbot(rules, history=history, memory_length=50)


class ChatRequest(BaseModel):
    message: str


class ChatResponse(BaseModel):
    response: str
    history: list[dict[str, str]]


app = FastAPI(title="Chatbot API", version="1.0.0")

_history_path = CURRENT_DIR / "session_history.json"
_bot = _build_bot(_history_path)


@app.post("/chat", response_model=ChatResponse)
def chat_endpoint(payload: ChatRequest) -> ChatResponse:
    message = payload.message.strip()
    if not message:
        raise HTTPException(status_code=400, detail="Message must not be empty")
    response = _bot.get_response(message)
    save_history(_history_path, _bot.history)
    return ChatResponse(
        response=response,
        history=[{"user": user, "bot": bot} for user, bot in _bot.history],
    )


@app.get("/health", response_model=dict)
def healthcheck() -> dict:
    """Lightweight endpoint for uptime checks."""

    return {"status": "ok", "memory_length": len(_bot.history)}
