"""Entry point for the Monster Raising simulator."""

from __future__ import annotations

from pathlib import Path
from typing import Callable, Dict, List, Optional, Tuple

import pygame

from monster_data import Monster, MonsterSpecies, STAT_NAMES, load_species_catalog
from simulation import GameState, load_or_create, quickstart_state

ROOT = Path(__file__).resolve().parent
DATA_PATH = ROOT / "data" / "species.json"
SAVE_PATH = ROOT / "saves" / "stable.json"
QUICKSTART_PATH = ROOT / "saves" / "quickstart.json"
WINDOW_SIZE = (960, 640)

BACKGROUND = (18, 24, 38)
PANEL = (32, 44, 68)
PANEL_DARK = (24, 32, 50)
ACCENT = (92, 136, 218)
ACCENT_DISABLED = (70, 70, 90)
TEXT_PRIMARY = (230, 236, 255)
TEXT_MUTED = (170, 182, 208)
MOOD_COLOURS = {
    "content": (120, 200, 140),
    "neutral": (120, 150, 200),
    "famished": (220, 120, 120),
    "tired": (200, 180, 90),
    "irritable": (210, 150, 80),
}


class Button:
    def __init__(self, label: str, callback: Callable[[], None]) -> None:
        self.label = label
        self.callback = callback
        self.rect = pygame.Rect(0, 0, 0, 0)
        self.enabled = True

    def draw(self, surface: pygame.Surface, font: pygame.font.Font) -> None:
        colour = ACCENT if self.enabled else ACCENT_DISABLED
        pygame.draw.rect(surface, colour, self.rect, border_radius=6)
        text = font.render(self.label, True, TEXT_PRIMARY)
        surface.blit(text, text.get_rect(center=self.rect.center))

    def handle_click(self, position: Tuple[int, int]) -> bool:
        if self.enabled and self.rect.collidepoint(position):
            self.callback()
            return True
        return False


def wrap_text(text: str, font: pygame.font.Font, max_width: int) -> List[str]:
    words = text.split()
    lines: List[str] = []
    current = ""
    for word in words:
        trial = f"{current} {word}".strip()
        if font.size(trial)[0] <= max_width:
            current = trial
        else:
            if current:
                lines.append(current)
            current = word
    if current:
        lines.append(current)
    return lines


def ensure_quickstart(species_catalog: Dict[str, MonsterSpecies]) -> None:
    if QUICKSTART_PATH.exists():
        return
    state = quickstart_state(species_catalog)
    state.save(QUICKSTART_PATH)


def compute_layout(
    width: int, height: int
) -> Tuple[pygame.Rect, pygame.Rect, pygame.Rect, pygame.Rect]:
    roster_rect = pygame.Rect(0, 0, int(width * 0.3), height - 120)
    log_rect = pygame.Rect(
        roster_rect.right, 0, width - roster_rect.width, int(height * 0.35)
    )
    detail_rect = pygame.Rect(
        roster_rect.right,
        log_rect.bottom,
        width - roster_rect.width,
        height - log_rect.height - 120,
    )
    action_rect = pygame.Rect(0, height - 120, width, 120)
    return roster_rect, log_rect, detail_rect, action_rect


def event_log_capacity(rect: pygame.Rect, font: pygame.font.Font) -> int:
    info_height = 34
    log_height = rect.height - info_height - 12
    line_height = font.get_height() + 4
    return max(1, log_height // line_height)


def main() -> None:
    pygame.init()
    pygame.display.set_caption("Monster Raising Simulator")
    screen = pygame.display.set_mode(WINDOW_SIZE, pygame.RESIZABLE)
    clock = pygame.time.Clock()

    font_small = pygame.font.Font(None, 24)
    font_medium = pygame.font.Font(None, 28)
    font_large = pygame.font.Font(None, 32)

    species_catalog = load_species_catalog(DATA_PATH)
    ensure_quickstart(species_catalog)
    state = load_or_create(SAVE_PATH, species_catalog)

    selected_index = 0
    status_message = "Welcome to the ranch!"
    training_focus = STAT_NAMES[0]
    feed_quality = "basic"
    log_offset = 0

    def reset_log() -> None:
        nonlocal log_offset
        log_offset = 0

    def run_action(action: Callable[[], str]) -> None:
        nonlocal status_message
        try:
            message = action()
        except Exception as exc:  # noqa: BLE001 - surface feedback is preferred
            status_message = str(exc)
            state.log_event(str(exc))
        else:
            status_message = message
            reset_log()

    def current_monster() -> Optional[Monster]:
        if 0 <= selected_index < len(state.monsters):
            return state.monsters[selected_index]
        return None

    def feed_monster_default() -> str:
        monster = current_monster()
        if not monster:
            raise RuntimeError("No monster selected.")
        return state.feed_monster(monster, feed_quality)

    def feed_monster_deluxe() -> str:
        monster = current_monster()
        if not monster:
            raise RuntimeError("No monster selected.")
        return state.feed_monster(monster, "deluxe")

    def train_monster() -> str:
        monster = current_monster()
        if not monster:
            raise RuntimeError("No monster selected.")
        return state.train_monster(monster, training_focus)

    def rest_monster() -> str:
        monster = current_monster()
        if not monster:
            raise RuntimeError("No monster selected.")
        return state.rest_monster(monster)

    def breed_monster() -> str:
        monster = current_monster()
        if not monster:
            raise RuntimeError("Select a monster first.")
        partner = state.select_best_partner(monster)
        if not partner:
            raise RuntimeError("No partner available. Adults required.")
        egg = state.breed_monsters(monster, partner)
        return (
            f"Egg incubating: {egg.species} (hatches in {egg.days_until_hatch} days)."
        )

    def advance_day() -> str:
        state.advance_day()
        return f"Advanced to day {state.day}."

    def save_game() -> str:
        state.save(SAVE_PATH)
        return "Progress saved to stable.json."

    def load_game() -> str:
        nonlocal state, selected_index
        if not SAVE_PATH.exists():
            raise FileNotFoundError("No stable.json save found yet.")
        state = GameState.load(species_catalog, SAVE_PATH)
        selected_index = 0 if state.monsters else -1
        return "Loaded stable.json."

    def load_quickstart() -> str:
        nonlocal state, selected_index
        if not QUICKSTART_PATH.exists():
            raise FileNotFoundError("quickstart.json missing.")
        state = GameState.load(species_catalog, QUICKSTART_PATH)
        selected_index = 0 if state.monsters else -1
        return "Quick-start scenario loaded."

    button_feed_basic = Button("Feed", lambda: run_action(feed_monster_default))
    button_feed_deluxe = Button("Deluxe Feed", lambda: run_action(feed_monster_deluxe))
    button_train = Button("Train", lambda: run_action(train_monster))
    button_rest = Button("Rest", lambda: run_action(rest_monster))
    button_breed = Button("Breed", lambda: run_action(breed_monster))
    button_next = Button("Next Day", lambda: run_action(advance_day))
    button_save = Button("Save", lambda: run_action(save_game))
    button_load = Button("Load", lambda: run_action(load_game))
    button_quick = Button("Quick-Start", lambda: run_action(load_quickstart))

    buttons = [
        button_feed_basic,
        button_feed_deluxe,
        button_train,
        button_rest,
        button_breed,
        button_next,
        button_save,
        button_load,
        button_quick,
    ]

    def available_feed_count(quality: str) -> int:
        key = "basic_feed" if quality == "basic" else "deluxe_feed"
        return state.inventory.get(key, 0)

    def update_button_states() -> None:
        monster = current_monster()
        button_feed_basic.enabled = (
            bool(monster) and available_feed_count(feed_quality) > 0
        )
        button_feed_deluxe.enabled = (
            bool(monster) and state.inventory.get("deluxe_feed", 0) > 0
        )
        button_train.enabled = bool(monster)
        button_rest.enabled = bool(monster)
        button_breed.enabled = (
            bool(monster) and state.select_best_partner(monster) is not None
        )
        button_next.enabled = True
        button_save.enabled = True
        button_load.enabled = SAVE_PATH.exists()
        button_quick.enabled = QUICKSTART_PATH.exists()

    running = True
    while running:
        dt = clock.tick(60)
        width, height = screen.get_size()
        roster_rect, log_rect, detail_rect, action_rect = compute_layout(width, height)
        update_button_states()

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
            elif event.type == pygame.VIDEORESIZE:
                screen = pygame.display.set_mode((event.w, event.h), pygame.RESIZABLE)
            elif event.type == pygame.MOUSEBUTTONDOWN:
                if event.button == 1:
                    new_index = roster_selection_from_pos(
                        event.pos, roster_rect, len(state.monsters), font_medium
                    )
                    if new_index is not None:
                        selected_index = new_index
                        status_message = (
                            f"Selected {state.monsters[selected_index].name}."
                        )
                    else:
                        for button in buttons:
                            if button.handle_click(event.pos):
                                break
                elif event.button == 4:
                    log_offset = max(0, log_offset - 1)
                elif event.button == 5:
                    capacity = event_log_capacity(log_rect, font_small)
                    max_offset = max(0, len(state.event_log) - capacity)
                    log_offset = min(max_offset, log_offset + 1)
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_ESCAPE:
                    running = False
                elif event.key == pygame.K_f:
                    run_action(feed_monster_default)
                elif event.key == pygame.K_d:
                    run_action(feed_monster_deluxe)
                elif event.key == pygame.K_t:
                    run_action(train_monster)
                elif event.key == pygame.K_r:
                    run_action(rest_monster)
                elif event.key == pygame.K_b:
                    run_action(breed_monster)
                elif event.key == pygame.K_n:
                    run_action(advance_day)
                elif event.key == pygame.K_s:
                    run_action(save_game)
                elif event.key == pygame.K_l:
                    run_action(load_game)
                elif event.key == pygame.K_q:
                    run_action(load_quickstart)
                elif pygame.K_1 <= event.key <= pygame.K_5:
                    index = event.key - pygame.K_1
                    training_focus = STAT_NAMES[index]
                    status_message = f"Training focus set to {training_focus}."
                elif event.key == pygame.K_p:
                    feed_quality = "deluxe" if feed_quality == "basic" else "basic"
                    status_message = f"Default feed set to {feed_quality}."

        selected_index = clamp_selection(selected_index, state)
        capacity = event_log_capacity(log_rect, font_small)
        max_offset = max(0, len(state.event_log) - capacity)
        log_offset = max(0, min(log_offset, max_offset))

        draw_ui(
            screen,
            state,
            roster_rect,
            log_rect,
            detail_rect,
            action_rect,
            selected_index,
            buttons,
            status_message,
            training_focus,
            feed_quality,
            font_small,
            font_medium,
            font_large,
            log_offset,
        )
        pygame.display.flip()

    pygame.quit()


def clamp_selection(index: int, state: GameState) -> int:
    if not state.monsters:
        return -1
    return max(0, min(index, len(state.monsters) - 1))


def roster_selection_from_pos(
    pos: Tuple[int, int], rect: pygame.Rect, count: int, font_medium: pygame.font.Font
) -> Optional[int]:
    if not rect.collidepoint(pos):
        return None
    margin = 10
    entry_height = 46
    title_height = font_medium.get_height()
    y_offset = rect.top + margin + title_height + 8
    for idx in range(count):
        entry_rect = pygame.Rect(
            rect.left + margin, y_offset, rect.width - margin * 2, entry_height
        )
        if entry_rect.collidepoint(pos):
            return idx
        y_offset += entry_height + 6
    return None


def draw_ui(
    screen: pygame.Surface,
    state: GameState,
    roster_rect: pygame.Rect,
    log_rect: pygame.Rect,
    detail_rect: pygame.Rect,
    action_rect: pygame.Rect,
    selected_index: int,
    buttons: List[Button],
    status_message: str,
    training_focus: str,
    feed_quality: str,
    font_small: pygame.font.Font,
    font_medium: pygame.font.Font,
    font_large: pygame.font.Font,
    log_offset: int,
) -> None:
    screen.fill(BACKGROUND)
    draw_roster(screen, roster_rect, state, selected_index, font_small, font_medium)
    draw_event_log(screen, log_rect, state, font_small, font_medium, log_offset)
    draw_details(
        screen,
        detail_rect,
        state,
        selected_index,
        font_small,
        font_medium,
        font_large,
        training_focus,
        feed_quality,
    )
    draw_action_bar(screen, action_rect, buttons, font_medium, status_message)


def draw_roster(
    surface: pygame.Surface,
    rect: pygame.Rect,
    state: GameState,
    selected_index: int,
    font_small: pygame.font.Font,
    font_medium: pygame.font.Font,
) -> None:
    pygame.draw.rect(surface, PANEL, rect)
    margin = 10
    title = font_medium.render("Stable", True, TEXT_PRIMARY)
    surface.blit(title, (rect.left + margin, rect.top + margin))
    y_offset = rect.top + margin + title.get_height() + 8
    entry_height = 46
    for idx, monster in enumerate(state.monsters):
        entry_rect = pygame.Rect(
            rect.left + margin, y_offset, rect.width - margin * 2, entry_height
        )
        colour = ACCENT if idx == selected_index else PANEL_DARK
        pygame.draw.rect(surface, colour, entry_rect, border_radius=6)
        name_text = font_medium.render(
            f"{monster.name} ({monster.species.name})", True, TEXT_PRIMARY
        )
        surface.blit(name_text, (entry_rect.left + 8, entry_rect.top + 6))
        mood = monster.mood()
        mood_colour = MOOD_COLOURS.get(mood, TEXT_MUTED)
        mood_text = font_small.render(f"Mood: {mood}", True, mood_colour)
        surface.blit(mood_text, (entry_rect.left + 8, entry_rect.top + 26))
        y_offset += entry_height + 6

    if state.eggs:
        eggs_title = font_medium.render("Incubator", True, TEXT_PRIMARY)
        surface.blit(eggs_title, (rect.left + margin, y_offset + 6))
        y_offset += eggs_title.get_height() + 10
        for egg in state.eggs:
            egg_text = font_small.render(
                f"{egg.species} egg — {egg.days_until_hatch} day(s)", True, TEXT_MUTED
            )
            surface.blit(egg_text, (rect.left + margin, y_offset))
            y_offset += egg_text.get_height() + 4


def draw_event_log(
    surface: pygame.Surface,
    rect: pygame.Rect,
    state: GameState,
    font_small: pygame.font.Font,
    font_medium: pygame.font.Font,
    log_offset: int,
) -> None:
    pygame.draw.rect(surface, PANEL, rect)
    info_height = 34
    info_rect = pygame.Rect(rect.left, rect.top, rect.width, info_height)
    pygame.draw.rect(surface, PANEL_DARK, info_rect)
    summary = font_medium.render(
        f"Day {state.day} | Gold: {state.gold} | Reputation: {state.reputation}",
        True,
        TEXT_PRIMARY,
    )
    surface.blit(summary, (info_rect.left + 10, info_rect.top + 6))
    inventory_text = font_small.render(
        f"Feed: basic={state.inventory.get('basic_feed', 0)} deluxe={state.inventory.get('deluxe_feed', 0)}",
        True,
        TEXT_MUTED,
    )
    surface.blit(inventory_text, (info_rect.left + 10, info_rect.top + 18))

    log_area = pygame.Rect(
        rect.left + 10,
        info_rect.bottom + 6,
        rect.width - 20,
        rect.height - info_height - 12,
    )
    pygame.draw.rect(surface, PANEL_DARK, log_area, border_radius=6)
    line_height = font_small.get_height() + 4
    max_entries = event_log_capacity(rect, font_small)
    events = list(state.event_log)
    start = max(0, len(events) - max_entries - log_offset)
    end = max(0, len(events) - log_offset)
    visible = events[start:end]
    y = log_area.bottom - line_height
    for event_text in reversed(visible):
        for line in reversed(wrap_text(event_text, font_small, log_area.width - 12)):
            rendered = font_small.render(line, True, TEXT_PRIMARY)
            surface.blit(rendered, (log_area.left + 6, y))
            y -= line_height
            if y < log_area.top:
                break
        if y < log_area.top:
            break


def draw_details(
    surface: pygame.Surface,
    rect: pygame.Rect,
    state: GameState,
    selected_index: int,
    font_small: pygame.font.Font,
    font_medium: pygame.font.Font,
    font_large: pygame.font.Font,
    training_focus: str,
    feed_quality: str,
) -> None:
    pygame.draw.rect(surface, PANEL, rect)
    margin = 14
    if not state.monsters or selected_index < 0:
        placeholder = font_medium.render(
            "Select a monster to view details", True, TEXT_MUTED
        )
        surface.blit(placeholder, placeholder.get_rect(center=rect.center))
        return
    monster = state.monsters[selected_index]
    header = font_large.render(
        f"{monster.name} — {monster.species.name}", True, TEXT_PRIMARY
    )
    surface.blit(header, (rect.left + margin, rect.top + margin))
    subline = font_small.render(
        f"Age: {monster.age_days} days ({monster.stage}) | Level {monster.level} | Temperament: {monster.species.temperament}",
        True,
        TEXT_MUTED,
    )
    surface.blit(
        subline, (rect.left + margin, rect.top + margin + header.get_height() + 4)
    )

    stats_y = rect.top + margin + header.get_height() + 32
    for stat in STAT_NAMES:
        value = monster.stats[stat]
        stat_text = font_medium.render(
            f"{stat.capitalize()}: {value:.1f}", True, TEXT_PRIMARY
        )
        surface.blit(stat_text, (rect.left + margin, stats_y))
        stats_y += stat_text.get_height() + 4

    needs_text = font_small.render(
        f"Energy {monster.energy:.0f} | Hunger {monster.hunger:.0f} | Happiness {monster.happiness:.0f} | Discipline {monster.discipline:.0f}",
        True,
        TEXT_MUTED,
    )
    surface.blit(needs_text, (rect.left + margin, stats_y + 6))
    stats_y += needs_text.get_height() + 24

    focus_text = font_small.render(
        f"Training focus: {training_focus} (press 1-5) | Pending feed: {feed_quality}",
        True,
        TEXT_MUTED,
    )
    surface.blit(focus_text, (rect.left + margin, stats_y))
    stats_y += focus_text.get_height() + 6

    log_label = font_small.render(
        f"Last activity: {monster.last_event}", True, TEXT_MUTED
    )
    surface.blit(log_label, (rect.left + margin, stats_y))


def draw_action_bar(
    surface: pygame.Surface,
    rect: pygame.Rect,
    buttons: List[Button],
    font_medium: pygame.font.Font,
    status_message: str,
) -> None:
    pygame.draw.rect(surface, PANEL, rect)
    status = font_medium.render(status_message, True, TEXT_PRIMARY)
    surface.blit(status, (rect.left + 16, rect.top + 10))

    button_area = pygame.Rect(
        rect.left + 16, rect.top + 48, rect.width - 32, rect.height - 56
    )
    rows = 2
    cols = (len(buttons) + rows - 1) // rows
    spacing_x = button_area.width // cols
    spacing_y = button_area.height // rows
    for idx, button in enumerate(buttons):
        col = idx % cols
        row = idx // cols
        button.rect = pygame.Rect(
            button_area.left + col * spacing_x + 6,
            button_area.top + row * spacing_y + 6,
            spacing_x - 12,
            spacing_y - 12,
        )
        button.draw(surface, font_medium)


if __name__ == "__main__":
    main()
