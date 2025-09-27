"""Primary game loop that ties together world, dialogue, and battle scenes."""

from __future__ import annotations

import random
from typing import Dict, List

import pygame

from .battle import BattleScene, create_enemy_from_data
from .constants import FPS, SCREEN_HEIGHT, SCREEN_WIDTH
from .data_loader import ensure_directories, load_data
from .dialogue import DialogueLine, DialogueTree, Quest, QuestObjective
from .entity import Entity, Inventory, Item, Stats
from .save_system import load_game, save_game
from .world import NPC, Player, TileMap, TileType


class Game:
    """A tiny but complete RPG gameplay loop built on reusable systems."""

    def __init__(self) -> None:
        pygame.init()
        ensure_directories()
        self.screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
        pygame.display.set_caption('RPG Engine Prototype')
        self.clock = pygame.time.Clock()
        self.font = pygame.font.SysFont('arial', 18)
        self.running = True
        self.state = 'overworld'
        self.dialogue_node: DialogueLine | None = None
        self.dialogue_tree: DialogueTree | None = None
        self.dialogue_node_id: str | None = None
        self.dialogue_resume_id: str | None = None
        self.battle_scene: BattleScene | None = None
        self.pending_battle_rewards: Dict[str, List[int]] = {}
        self.claimed_dialogue_rewards: set[tuple[str, str]] = set()
        self.space_down = False

        self.tile_map: TileMap | None = None
        self.player: Player | None = None
        self.player_entity: Entity | None = None
        self.npcs: List[NPC] = []
        self.dialogues: Dict[str, DialogueTree] = {}
        self.quests: Dict[str, Quest] = {}
        self.items: Dict[str, Item] = {}
        self.enemies: Dict[str, Dict[str, object]] = {}
        self.encounters: Dict[str, Dict[str, object]] = {}

        self.load_content()
        self.try_resume_save()

    # ------------------------------------------------------------------
    # Content loading and persistence
    # ------------------------------------------------------------------
    def load_content(self) -> None:
        world_data = load_data('world.json')
        tile_types: Dict[str, TileType] = {}
        for tile_id, tile_info in world_data['tiles'].items():
            tile_types[tile_id] = TileType(
                tile_id=tile_id,
                color=tuple(tile_info['color']),
                walkable=bool(tile_info['walkable']),
                encounter_rate=float(tile_info.get('encounter_rate', 0.0)),
                encounters=list(tile_info.get('encounters', [])),
            )
        self.tile_map = TileMap(world_data['map'], tile_types)
        self.player = Player(tuple(world_data['player_start']))
        self.encounters = {
            tile_id: {
                'rate': tile.encounter_rate,
                'encounters': tile.encounters,
            }
            for tile_id, tile in tile_types.items()
            if tile.encounter_rate > 0 and tile.encounters
        }

        self.npcs = [
            NPC(
                npc_id=npc['id'],
                name=npc['name'],
                position=tuple(npc['position']),
                dialogue_id=npc['dialogue'],
                quest_id=npc.get('quest'),
            )
            for npc in load_data('npcs.json')
        ]

        dialogue_data = load_data('dialogue.yaml')
        for dialogue_id, nodes in dialogue_data.items():
            parsed_nodes = {
                node_id: DialogueLine(
                    speaker=node_info.get('speaker', ''),
                    text=node_info['text'],
                    next_id=node_info.get('next'),
                    triggers_battle=node_info.get('battle'),
                    complete_objectives={
                        qid: list(indices)
                        for qid, indices in node_info.get('complete_objectives', {}).items()
                    },
                    reward_objectives={
                        qid: list(indices)
                        for qid, indices in node_info.get('reward_objectives', {}).items()
                    },
                    give_items=dict(node_info.get('give_items', {})),
                )
                for node_id, node_info in nodes.items()
            }
            self.dialogues[dialogue_id] = DialogueTree(dialogue_id, parsed_nodes)

        quest_data = load_data('quests.json')
        for quest_id, quest_info in quest_data.items():
            objectives = [QuestObjective(text) for text in quest_info['objectives']]
            self.quests[quest_id] = Quest(
                quest_id=quest_id,
                title=quest_info['title'],
                description=quest_info['description'],
                objectives=objectives,
                rewards=quest_info.get('rewards', {}),
            )

        item_data = load_data('items.json')
        self.items = {
            item_id: Item(
                name=item_info['name'],
                description=item_info['description'],
                heal_amount=item_info.get('heal', 0),
                attack_boost=item_info.get('attack', 0),
                defense_boost=item_info.get('defense', 0),
            )
            for item_id, item_info in item_data.items()
        }

        player_data = load_data('player.json')
        stats = Stats(**player_data['stats'])
        inventory = Inventory(player_data.get('inventory', {}))
        self.player_entity = Entity('hero', player_data['name'], stats, inventory)

        self.enemies = {enemy['id']: enemy for enemy in load_data('enemies.json')}

    def try_resume_save(self) -> None:
        save_data = load_game()
        if not save_data or not self.player_entity:
            return
        self.player_entity = Entity.from_dict(save_data['player'])
        if self.player:
            x, y = save_data['world'].get('player_pos', [self.player.x, self.player.y])
            self.player.x, self.player.y = x, y
        for quest_id, quest_state in save_data['world'].get('quests', {}).items():
            quest = self.quests.get(quest_id)
            if not quest:
                continue
            for idx, done in enumerate(quest_state.get('objectives', [])):
                if done:
                    quest.complete_objective(idx)
            quest.completed = quest_state.get('completed', quest.completed)

    def save(self) -> None:
        if not self.player or not self.player_entity:
            return
        quest_state = {
            quest_id: {
                'completed': quest.completed,
                'objectives': [obj.complete for obj in quest.objectives],
            }
            for quest_id, quest in self.quests.items()
        }
        world_state = {
            'player_pos': [self.player.x, self.player.y],
            'quests': quest_state,
        }
        save_game(self.player_entity, world_state)

    # ------------------------------------------------------------------
    # Main loop and state updates
    # ------------------------------------------------------------------
    def run(self) -> None:
        while self.running:
            self.clock.tick(FPS)
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.running = False
                elif self.state == 'battle' and self.battle_scene:
                    self.battle_scene.handle_input(event)
                elif event.type == pygame.KEYDOWN:
                    if event.key == pygame.K_ESCAPE:
                        self.running = False
                    elif event.key == pygame.K_s:
                        self.save()
                    elif event.key == pygame.K_l:
                        self.try_resume_save()
            if self.state == 'overworld':
                self.update_overworld()
                self.draw_overworld()
            elif self.state == 'dialogue':
                self.draw_dialogue()
            elif self.state == 'battle' and self.battle_scene:
                self.draw_battle()
            pygame.display.flip()
        pygame.quit()

    def update_overworld(self) -> None:
        assert self.tile_map and self.player and self.player_entity
        keys = pygame.key.get_pressed()
        moved = False
        if keys[pygame.K_LEFT] or keys[pygame.K_a]:
            moved = self.player.move(-1, 0, self.tile_map)
        elif keys[pygame.K_RIGHT] or keys[pygame.K_d]:
            moved = self.player.move(1, 0, self.tile_map)
        elif keys[pygame.K_UP] or keys[pygame.K_w]:
            moved = self.player.move(0, -1, self.tile_map)
        elif keys[pygame.K_DOWN] or keys[pygame.K_s]:
            moved = self.player.move(0, 1, self.tile_map)

        if moved:
            tile = self.tile_map.get_tile(self.player.x, self.player.y)
            if tile and tile.tile_id in self.encounters:
                encounter_info = self.encounters[tile.tile_id]
                if random.random() < encounter_info['rate']:
                    enemy_id = random.choice(encounter_info['encounters'])
                    self.start_battle(enemy_id)

        if keys[pygame.K_SPACE] and not self.space_down:
            npc = self.get_nearby_npc()
            if npc:
                self.start_dialogue(npc)
            self.space_down = True
        elif not keys[pygame.K_SPACE]:
            self.space_down = False

    def draw_overworld(self) -> None:
        assert self.tile_map and self.player
        self.tile_map.draw(self.screen)
        for npc in self.npcs:
            self.screen.fill((200, 200, 50), npc.rect())
        self.player.draw(self.screen)
        for idx, quest in enumerate(self.quests.values()):
            status = 'Done' if quest.completed else 'In progress'
            text = self.font.render(f'{quest.title}: {status}', True, (255, 255, 255))
            self.screen.blit(text, (16, SCREEN_HEIGHT - 20 * (idx + 1)))

    # ------------------------------------------------------------------
    # Dialogue handling
    # ------------------------------------------------------------------
    def get_nearby_npc(self) -> NPC | None:
        assert self.player
        for npc in self.npcs:
            if abs(npc.position[0] - self.player.x) + abs(npc.position[1] - self.player.y) == 1:
                return npc
        return None

    def start_dialogue(self, npc: NPC) -> None:
        dialogue = self.dialogues.get(npc.dialogue_id)
        if not dialogue:
            return
        self.dialogue_tree = dialogue
        self.dialogue_node = dialogue.start()
        self.dialogue_node_id = 'start'
        self.state = 'dialogue'
        self.dialogue_resume_id = None
        self.space_down = True
        self.apply_dialogue_effects(self.dialogue_node, 'start')

    def draw_dialogue(self) -> None:
        assert self.dialogue_node
        self.screen.fill((30, 30, 30))
        text = self.font.render(f"{self.dialogue_node.speaker}: {self.dialogue_node.text}", True, (255, 255, 255))
        self.screen.blit(text, (32, SCREEN_HEIGHT // 2))
        prompt = self.font.render('Press SPACE to continue', True, (180, 180, 180))
        self.screen.blit(prompt, (32, SCREEN_HEIGHT // 2 + 40))
        keys = pygame.key.get_pressed()
        if keys[pygame.K_SPACE] and not self.space_down:
            self.space_down = True
            self.advance_dialogue()
        elif not keys[pygame.K_SPACE]:
            self.space_down = False

    def advance_dialogue(self) -> None:
        assert self.dialogue_node and self.dialogue_tree
        node = self.dialogue_node
        if node.triggers_battle:
            self.pending_battle_rewards = node.reward_objectives
            self.dialogue_resume_id = node.next_id
            self.start_battle(node.triggers_battle)
            return
        next_id = node.next_id
        next_node = self.dialogue_tree.next(node)
        if next_node is None or next_id is None:
            self.dialogue_tree = None
            self.dialogue_node = None
            self.dialogue_node_id = None
            self.dialogue_resume_id = None
            self.state = 'overworld'
            return
        self.dialogue_node = next_node
        self.dialogue_node_id = next_id
        self.apply_dialogue_effects(next_node, next_id)

    # ------------------------------------------------------------------
    # Battle handling
    # ------------------------------------------------------------------
    def start_battle(self, enemy_id: str) -> None:
        assert self.player_entity
        enemy_data = self.enemies.get(enemy_id)
        if not enemy_data:
            return
        self.battle_scene = BattleScene(self.player_entity, create_enemy_from_data(enemy_data), self.items)
        self.state = 'battle'
        self.space_down = True

    def draw_battle(self) -> None:
        assert self.battle_scene
        self.battle_scene.draw(self.screen)
        if self.battle_scene.ended:
            prompt = self.font.render('Press SPACE to continue', True, (255, 255, 255))
            self.screen.blit(prompt, (SCREEN_WIDTH // 2 - 110, SCREEN_HEIGHT - 40))
            keys = pygame.key.get_pressed()
            if keys[pygame.K_SPACE] and not self.space_down:
                self.space_down = True
                victory = self.battle_scene.victory
                if victory:
                    self.apply_objective_updates(self.pending_battle_rewards)
                else:
                    self.pending_battle_rewards = {}
                resume_id = self.dialogue_resume_id if victory else None
                resume_tree = self.dialogue_tree if victory else None
                self.pending_battle_rewards = {}
                self.dialogue_resume_id = None
                if victory and resume_tree and resume_id and resume_id in resume_tree.nodes:
                    next_node = resume_tree.nodes[resume_id]
                    self.dialogue_node = next_node
                    self.dialogue_node_id = resume_id
                    self.state = 'dialogue'
                    self.apply_dialogue_effects(next_node, resume_id)
                else:
                    if not victory:
                        self.dialogue_tree = None
                        self.dialogue_node = None
                        self.dialogue_node_id = None
                    self.state = 'overworld'
                self.battle_scene = None
            elif not keys[pygame.K_SPACE]:
                self.space_down = False

    # ------------------------------------------------------------------
    # Quest helpers
    # ------------------------------------------------------------------
    def apply_objective_updates(self, updates: Dict[str, List[int]]) -> None:
        for quest_id, indices in updates.items():
            quest = self.quests.get(quest_id)
            if not quest:
                continue
            for idx in indices:
                quest.complete_objective(idx)

    def apply_dialogue_effects(self, node: DialogueLine, node_id: str) -> None:
        self.apply_objective_updates(node.complete_objectives)
        if not node.give_items or not self.dialogue_tree:
            return
        key = (self.dialogue_tree.dialogue_id, node_id)
        if key in self.claimed_dialogue_rewards:
            return
        self.grant_items(node.give_items)
        self.claimed_dialogue_rewards.add(key)

    def grant_items(self, grants: Dict[str, int]) -> None:
        if not self.player_entity:
            return
        for item_id, amount in grants.items():
            if item_id not in self.items or amount <= 0:
                continue
            self.player_entity.inventory.add(item_id, amount)

