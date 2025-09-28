"""Dialogue scripting and quest state helpers."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List, Optional


@dataclass
class DialogueLine:
    speaker: str
    text: str
    next_id: Optional[str] = None
    triggers_battle: Optional[str] = None
    complete_objectives: Dict[str, List[int]] = field(default_factory=dict)
    reward_objectives: Dict[str, List[int]] = field(default_factory=dict)
    give_items: Dict[str, int] = field(default_factory=dict)


class DialogueTree:
    def __init__(self, dialogue_id: str, nodes: Dict[str, DialogueLine]):
        self.dialogue_id = dialogue_id
        self.nodes = nodes

    def start(self) -> DialogueLine:
        return self.nodes["start"]

    def next(self, node: DialogueLine) -> DialogueLine | None:
        if node.next_id is None:
            return None
        return self.nodes.get(node.next_id)


@dataclass
class QuestObjective:
    description: str
    complete: bool = False


@dataclass
class Quest:
    quest_id: str
    title: str
    description: str
    objectives: List[QuestObjective]
    rewards: Dict[str, int]
    completed: bool = False

    def check_completion(self) -> None:
        self.completed = all(obj.complete for obj in self.objectives)

    def complete_objective(self, index: int) -> None:
        if 0 <= index < len(self.objectives):
            self.objectives[index].complete = True
            self.check_completion()
