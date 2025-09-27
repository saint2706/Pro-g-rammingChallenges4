"""Simple message log for the roguelike."""
from __future__ import annotations

from collections import deque
from dataclasses import dataclass
from typing import Deque, Iterable, List, Tuple

Colour = Tuple[int, int, int]


@dataclass
class Message:
    text: str
    colour: Colour


class MessageLog:
    def __init__(self, *, max_length: int = 30) -> None:
        self.messages: Deque[Message] = deque(maxlen=max_length)

    def add(self, text: str, colour: Colour = (255, 255, 255)) -> None:
        self.messages.append(Message(text, colour))

    def render(self) -> List[Message]:
        return list(self.messages)
