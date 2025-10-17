"""Simple menu-driven battle scene implementation."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, List, Tuple

import pygame

from .constants import FONT_NAME, SCREEN_HEIGHT, SCREEN_WIDTH
from .entity import Entity, Item, Stats


@dataclass
class BattleAction:
    label: str
    description: str


class BattleScene:
    def __init__(self, player: Entity, enemy: Entity, items: Dict[str, Item]):
        self.player = player
        self.enemy = enemy
        self.items = items
        self.actions: List[BattleAction] = [
            BattleAction("Attack", "Strike the enemy with your weapon."),
            BattleAction("Item", "Use a healing potion from your pack."),
            BattleAction("Flee", "Attempt to run from battle."),
        ]
        self.selection_index = 0
        self.log: List[str] = [f"A wild {self.enemy.name} appears!"]
        self.font = pygame.font.SysFont(FONT_NAME, 18)
        self.ended = False
        self.victory = False

    def draw(self, surface: pygame.Surface) -> None:
        surface.fill((0, 0, 0))
        self._draw_status(surface)
        self._draw_actions(surface)
        self._draw_log(surface)

    def _draw_status(self, surface: pygame.Surface) -> None:
        player_text = self.font.render(
            f"{self.player.name} HP: {self.player.stats.hp}/{self.player.stats.max_hp}",
            True,
            (255, 255, 255),
        )
        enemy_text = self.font.render(
            f"{self.enemy.name} HP: {self.enemy.stats.hp}/{self.enemy.stats.max_hp}",
            True,
            (255, 120, 120),
        )
        surface.blit(player_text, (32, 32))
        surface.blit(enemy_text, (32, 64))

    def _draw_actions(self, surface: pygame.Surface) -> None:
        for i, action in enumerate(self.actions):
            color = (255, 255, 0) if i == self.selection_index else (200, 200, 200)
            text = self.font.render(action.label, True, color)
            surface.blit(text, (32, SCREEN_HEIGHT - 150 + i * 24))

    def _draw_log(self, surface: pygame.Surface) -> None:
        base_y = SCREEN_HEIGHT - 90
        recent = self.log[-3:]
        for i, entry in enumerate(recent):
            text = self.font.render(entry, True, (180, 180, 180))
            surface.blit(text, (32, base_y + i * 20))

    def handle_input(self, event: pygame.event.Event) -> None:
        if event.type == pygame.KEYDOWN:
            if event.key in {pygame.K_UP, pygame.K_w}:
                self.selection_index = (self.selection_index - 1) % len(self.actions)
            elif event.key in {pygame.K_DOWN, pygame.K_s}:
                self.selection_index = (self.selection_index + 1) % len(self.actions)
            elif event.key in {pygame.K_RETURN, pygame.K_SPACE}:
                self._execute_action(self.actions[self.selection_index])

    def _execute_action(self, action: BattleAction) -> None:
        if self.ended:
            return
        if action.label == "Attack":
            self._player_attack()
        elif action.label == "Item":
            self._use_item()
        elif action.label == "Flee":
            self.log.append("You fled from battle!")
            self.ended = True
        self._check_end_conditions()
        if not self.ended:
            self._enemy_turn()
            self._check_end_conditions()

    def _player_attack(self) -> None:
        damage = max(1, self.player.stats.attack - self.enemy.stats.defense // 2)
        dealt = self.enemy.stats.take_damage(damage)
        self.log.append(f"You deal {dealt} damage to {self.enemy.name}!")

    def _use_item(self) -> None:
        consumables = [
            item_id
            for item_id in self.player.inventory.list_items()
            if self.items[item_id].heal_amount > 0
        ]
        if not consumables:
            self.log.append("No healing items available!")
            return
        item_id = consumables[0]
        item = self.items[item_id]
        self.player.inventory.remove(item_id)
        self.player.stats.heal(item.heal_amount)
        self.log.append(f"You use {item.name} and recover {item.heal_amount} HP!")

    def _enemy_turn(self) -> None:
        if not self.enemy.stats.is_alive():
            return
        damage = max(1, self.enemy.stats.attack - self.player.stats.defense // 2)
        taken = self.player.stats.take_damage(damage)
        self.log.append(f"{self.enemy.name} hits you for {taken} damage!")

    def _check_end_conditions(self) -> None:
        if not self.enemy.stats.is_alive():
            self.log.append(f"{self.enemy.name} is defeated!")
            self.ended = True
            self.victory = True
        elif not self.player.stats.is_alive():
            self.log.append("You have fallen in battle...")
            self.ended = True


def create_enemy_from_data(enemy_data: Dict[str, object]) -> Entity:
    stats_info = enemy_data["stats"]
    stats = Stats(
        max_hp=int(stats_info["max_hp"]),
        attack=int(stats_info["attack"]),
        defense=int(stats_info["defense"]),
        speed=int(stats_info["speed"]),
    )
    return Entity(
        entity_id=str(enemy_data["id"]), name=str(enemy_data["name"]), stats=stats
    )
