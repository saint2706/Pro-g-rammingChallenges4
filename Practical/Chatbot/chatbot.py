"""chatbot.py – Minimal rule-based console chatbot.

Enhancements:
  * Config dataclass & argparse CLI
  * External JSON rules file support (--rules)
  * Deterministic randomness via --seed
  * Exit phrase customization (--exit-phrase)
  * Optional single-turn mode (--once)
  * Token-based multi-keyword matching
  * Prevent immediate response repetition
  * Optional ANSI coloring (--no-color to disable)
  * History tracking (last N exchanges) and rule dump (--dump-rules)

Example:
  python chatbot.py --rules rules.json --seed 123
  python chatbot.py --once "What is your name?"
"""

from __future__ import annotations

import argparse
import json
import random
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Sequence, Optional, Iterable

# --------------------------- Data Structures --------------------------- #


@dataclass(slots=True)
class Config:
    rules_path: Optional[Path]
    seed: Optional[int]
    once: Optional[str]
    exit_phrase: str
    color: bool
    dump_rules: bool
    history_path: Optional[Path]
    memory_length: int
    export_history: Optional[Path]
    import_history: Optional[Path]


@dataclass(slots=True)
class RuleSet:
    responses: Dict[str, List[str]] = field(default_factory=dict)

    def ensure_default(self) -> None:
        if "default" not in self.responses:
            self.responses["default"] = ["Sorry, I don't understand."]


# --------------------------- Chatbot Core --------------------------- #


class SimpleChatbot:
    """Rule-based chatbot performing case-insensitive substring / token matching.

    Matching strategy:
      1. Normalize input -> lowercase
      2. Exact multi-word trigger if all tokens present (space-separated trigger string)
      3. Fallback to substring presence for single-word triggers
      4. Fallback to 'default'
    """

    def __init__(
        self,
        rules: RuleSet,
        *,
        history: Optional[Iterable[tuple[str, str]]] = None,
        memory_length: int = 100,
    ):
        self.rules = rules
        self.last_response: Optional[str] = None
        self.memory_length = max(1, memory_length)
        incoming: List[tuple[str, str]] = []
        if history is not None:
            incoming = [(str(u), str(b)) for u, b in history]
        self.history: List[tuple[str, str]] = incoming[-self.memory_length :]
        if self.history:
            self.last_response = self.history[-1][1]

    def _candidate_triggers(self) -> List[str]:
        return [k for k in self.rules.responses.keys() if k != "default"]

    def _match_trigger(self, user_input: str) -> str:
        text = user_input.lower().strip()
        if not text:
            return "default"
        tokens = text.split()
        # Multi-word triggers first (all words must appear)
        multi = [t for t in self._candidate_triggers() if " " in t]
        for trig in multi:
            parts = trig.split()
            if all(p in tokens for p in parts):
                return trig
        # Single word / substring fallback
        for trig in self._candidate_triggers():
            if trig in text:
                return trig
        return "default"

    def get_response(self, user_input: str) -> str:
        trig = self._match_trigger(user_input)
        options = self.rules.responses.get(trig, self.rules.responses["default"])
        if not options:
            return "..."  # safeguard
        # Avoid repeating last response if >1 choice
        if self.last_response and len(options) > 1:
            filtered = [o for o in options if o != self.last_response]
            if filtered:
                options = filtered
        choice = random.choice(options)
        self.last_response = choice
        self.history.append((user_input, choice))
        if len(self.history) > self.memory_length:
            self.history = self.history[-self.memory_length :]
        return choice


# --------------------------- CLI / IO --------------------------- #

DEFAULT_RULES: Dict[str, List[str]] = {
    "hello": ["Hello there!", "Hi! I'm happy to chat.", "Good to see you!"],
    "how are you": [
        "I'm just a bot, but I'm doing great!",
        "I'm functioning as expected, thanks for asking!",
    ],
    "name": [
        "You can call me a simple ChatBot.",
        "I don't have a name, I'm just a humble Python script.",
    ],
    "bye": ["Goodbye!", "See you later!", "Bye-bye!"],
    "help": ["Try keywords like 'hello', 'how are you', 'name', and 'bye'."],
    "default": [
        "Sorry, I don't understand that.",
        "Could you please rephrase?",
        "I'm not sure how to respond to that. Try asking for 'help'.",
    ],
}

ANSI_BOT = "\x1b[36mBot:\x1b[0m"
ANSI_USER = "\x1b[33mYou:\x1b[0m"


def load_rules(path: Optional[Path]) -> RuleSet:
    if path is None:
        return RuleSet(responses=DEFAULT_RULES.copy())
    try:
        data = json.loads(Path(path).read_text(encoding="utf-8"))
        if not isinstance(data, dict):
            raise ValueError(
                "Rules JSON must be an object mapping triggers to list of responses."
            )
        casted: Dict[str, List[str]] = {}
        for k, v in data.items():
            if isinstance(v, list):
                casted[k.lower()] = [str(x) for x in v]
        rs = RuleSet(casted)
        rs.ensure_default()
        return rs
    except Exception as e:
        print(f"Error loading rules file '{path}': {e}", file=sys.stderr)
        return RuleSet(responses=DEFAULT_RULES.copy())


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description="Simple rule-based chatbot")
    p.add_argument("--rules", type=Path, help="Path to JSON rules file")
    p.add_argument("--seed", type=int, help="Random seed for deterministic responses")
    p.add_argument("--once", help="Provide a single prompt and exit with response")
    p.add_argument(
        "--exit-phrase",
        default="bye",
        help="Exit phrase to terminate chat (default: bye)",
    )
    p.add_argument(
        "--no-color", action="store_true", help="Disable ANSI colors in output"
    )
    p.add_argument(
        "--dump-rules", action="store_true", help="Print loaded rules and exit"
    )
    p.add_argument(
        "--history-path",
        type=Path,
        default=Path.home() / ".simple_chatbot_history.json",
        help="Path to JSON file used to persist chat history (default: ~/.simple_chatbot_history.json)",
    )
    p.add_argument(
        "--memory-length",
        type=int,
        default=100,
        help="Maximum number of exchanges to keep in memory",
    )
    p.add_argument(
        "--export-history",
        type=Path,
        help="Write the current history to the given JSON file and exit",
    )
    p.add_argument(
        "--import-history",
        type=Path,
        help="Load history from the given JSON file before chatting",
    )
    return p


def parse_args(argv: Optional[Sequence[str]]) -> Config:
    parser = build_parser()
    args = parser.parse_args(argv)
    return Config(
        rules_path=args.rules,
        seed=args.seed,
        once=args.once,
        exit_phrase=args.exit_phrase.lower(),
        color=not args.no_color,
        dump_rules=args.dump_rules,
        history_path=args.history_path,
        memory_length=max(1, args.memory_length),
        export_history=args.export_history,
        import_history=args.import_history,
    )


def _coerce_history_items(items: Iterable[object]) -> List[tuple[str, str]]:
    history: List[tuple[str, str]] = []
    for item in items:
        if isinstance(item, dict):
            user = item.get("user")
            bot = item.get("bot")
            if user is None or bot is None:
                continue
            history.append((str(user), str(bot)))
        elif isinstance(item, (list, tuple)) and len(item) == 2:
            history.append((str(item[0]), str(item[1])))
    return history


def load_history(path: Optional[Path]) -> List[tuple[str, str]]:
    if path is None:
        return []
    try:
        if not path.exists():
            return []
        data = json.loads(path.read_text(encoding="utf-8"))
        if isinstance(data, list):
            return _coerce_history_items(data)
    except Exception:
        pass
    return []


def save_history(path: Optional[Path], history: Sequence[tuple[str, str]]) -> None:
    if path is None:
        return
    try:
        path.parent.mkdir(parents=True, exist_ok=True)
        serializable = [{"user": user, "bot": bot} for user, bot in history]
        path.write_text(json.dumps(serializable, indent=2), encoding="utf-8")
    except Exception as exc:
        print(f"Warning: could not persist history to {path}: {exc}", file=sys.stderr)


def export_history(history: Sequence[tuple[str, str]], destination: Path) -> None:
    save_history(destination, history)


def import_history(path: Path) -> List[tuple[str, str]]:
    return load_history(path)


def format_prompt(label: str, color: bool) -> str:
    if not color:
        return label.rstrip(":") + ":"
    return ANSI_USER if label.lower().startswith("you") else ANSI_BOT


def interactive(bot: SimpleChatbot, cfg: Config) -> int:
    print("--- Simple Rule-Based Chatbot ---")
    print(f"Type '{cfg.exit_phrase}' to exit. Try 'help' for hints.")
    while True:
        try:
            user_input = input(format_prompt("You:", cfg.color) + " ")
        except (EOFError, KeyboardInterrupt):
            print("\nGoodbye!")
            return 0
        if user_input.lower().strip() == cfg.exit_phrase:
            farewell = bot.get_response("bye")
            print(format_prompt("Bot:", cfg.color) + " " + farewell)
            save_history(cfg.history_path, bot.history)
            return 0
        response = bot.get_response(user_input)
        print(format_prompt("Bot:", cfg.color) + " " + response)
        save_history(cfg.history_path, bot.history)


def main(argv: Optional[Sequence[str]] = None) -> int:
    cfg = parse_args(argv)
    if cfg.seed is not None:
        random.seed(cfg.seed)
    rules = load_rules(cfg.rules_path)
    if cfg.dump_rules:
        print(json.dumps(rules.responses, indent=2))
        return 0

    history: List[tuple[str, str]] = load_history(cfg.history_path)
    if cfg.import_history is not None:
        history = import_history(cfg.import_history)
        save_history(cfg.history_path, history)
    bot = SimpleChatbot(rules, history=history, memory_length=cfg.memory_length)

    if cfg.export_history is not None and cfg.once is None:
        export_history(bot.history, cfg.export_history)
        return 0
    if cfg.once is not None:
        response = bot.get_response(cfg.once)
        print(response)
        save_history(cfg.history_path, bot.history)
        if cfg.export_history is not None:
            export_history(bot.history, cfg.export_history)
        return 0
    return interactive(bot, cfg)


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
