from __future__ import annotations

import json
import importlib.util
import sys
import random
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
MODULE_PATH = ROOT / "Practical" / "Chatbot" / "chatbot.py"
spec = importlib.util.spec_from_file_location("chatbot_module", MODULE_PATH)
if spec is None or spec.loader is None:  # pragma: no cover - defensive
    raise RuntimeError("Unable to load chatbot module")
chatbot = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = chatbot
spec.loader.exec_module(chatbot)

RuleSet = chatbot.RuleSet
SimpleChatbot = chatbot.SimpleChatbot
export_history = chatbot.export_history
import_history = chatbot.import_history
load_history = chatbot.load_history
save_history = chatbot.save_history


@pytest.fixture()
def sample_rules() -> RuleSet:
    rules = RuleSet(
        responses={"hello": ["Hi", "Greetings"], "bye": ["Bye"], "default": ["..."]}
    )
    rules.ensure_default()
    return rules


def test_history_persistence_across_sessions(
    tmp_path: Path, sample_rules: RuleSet
) -> None:
    history_path = tmp_path / "history.json"

    random.seed(123)
    bot_first = SimpleChatbot(sample_rules, memory_length=5)
    first_response = bot_first.get_response("hello")
    save_history(history_path, bot_first.history)

    loaded_history = load_history(history_path)
    assert loaded_history == bot_first.history

    random.seed(123)
    bot_second = SimpleChatbot(sample_rules, history=loaded_history, memory_length=5)
    assert bot_second.history == bot_first.history
    assert bot_second.last_response == first_response

    second_response = bot_second.get_response("hello")
    assert second_response != first_response  # avoided immediate repetition


def test_memory_length_enforced(tmp_path: Path, sample_rules: RuleSet) -> None:
    bot = SimpleChatbot(sample_rules, memory_length=3)
    for idx in range(5):
        bot.get_response(f"hello {idx}")
    assert len(bot.history) == 3
    assert bot.history[0][0] == "hello 2"

    history_path = tmp_path / "history_trimmed.json"
    save_history(history_path, bot.history)
    reloaded = load_history(history_path)
    assert len(reloaded) == 3


def test_import_export_round_trip(tmp_path: Path, sample_rules: RuleSet) -> None:
    bot = SimpleChatbot(sample_rules)
    bot.get_response("hello")
    bot.get_response("bye")

    export_target = tmp_path / "export.json"
    export_history(bot.history, export_target)

    assert json.loads(export_target.read_text(encoding="utf-8")) == [
        {"user": exchange[0], "bot": exchange[1]} for exchange in bot.history
    ]

    imported_history = import_history(export_target)
    new_bot = SimpleChatbot(sample_rules, history=imported_history)
    assert new_bot.history == bot.history
