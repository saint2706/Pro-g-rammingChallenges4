import argparse
import json
import math
import os
import random
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple

import pygame

Vector = pygame.math.Vector2


@dataclass
class Agent:
    ident: int
    position: Vector
    velocity: Vector
    max_speed: float
    detection_radius: float
    state: str = "runner"
    colour: Tuple[int, int, int] = (80, 200, 120)
    tag_times: List[float] = field(default_factory=list)

    def steer_seek(self, target: Vector) -> Vector:
        desired = target - self.position
        if desired.length_squared() == 0:
            return Vector()
        desired = desired.normalize() * self.max_speed
        steer = desired - self.velocity
        return steer

    def steer_flee(self, target: Vector) -> Vector:
        desired = self.position - target
        if desired.length_squared() == 0:
            return Vector()
        desired = desired.normalize() * self.max_speed
        steer = desired - self.velocity
        return steer

    def steer_wander(self) -> Vector:
        angle = random.uniform(0, math.tau)
        desired = Vector(math.cos(angle), math.sin(angle)) * (self.max_speed * 0.5)
        return desired - self.velocity


@dataclass
class TagEvent:
    time: float
    step: int
    hunter: int
    runner: int


class Arena:
    def __init__(
        self, size: Tuple[int, int], obstacle_count: int, seed: Optional[int] = None
    ) -> None:
        self.size = size
        rng = random.Random(seed)
        self.obstacles: List[pygame.Rect] = []
        width, height = size
        for _ in range(obstacle_count):
            w = rng.randint(60, 140)
            h = rng.randint(60, 140)
            x = rng.randint(40, width - w - 40)
            y = rng.randint(40, height - h - 40)
            self.obstacles.append(pygame.Rect(x, y, w, h))

    def wrap(self, position: Vector) -> Vector:
        x = position.x % self.size[0]
        y = position.y % self.size[1]
        return Vector(x, y)


class TagSimulation:
    def __init__(
        self,
        arena: Arena,
        agent_count: int,
        speed_variance: float,
        detection_radius: float,
        seed: Optional[int] = None,
        tag_distance: float = 20.0,
    ) -> None:
        if agent_count < 2:
            raise ValueError("Need at least two agents for tag")
        self.arena = arena
        self.agent_count = agent_count
        self.speed_variance = speed_variance
        self.detection_radius = detection_radius
        self.tag_distance = tag_distance
        self.rng = random.Random(seed)
        self.agents: List[Agent] = []
        self.events: List[TagEvent] = []
        self.tag_order: List[int] = []
        self.time_elapsed = 0.0
        self._init_agents()

    def _init_agents(self) -> None:
        width, height = self.arena.size
        base_speed = 120.0
        for ident in range(self.agent_count):
            position = Vector(
                self.rng.uniform(40, width - 40),
                self.rng.uniform(40, height - 40),
            )
            heading = Vector(self.rng.uniform(-1, 1), self.rng.uniform(-1, 1))
            if heading.length_squared() == 0:
                heading = Vector(1, 0)
            heading = heading.normalize()
            variance = self.rng.uniform(-self.speed_variance, self.speed_variance)
            max_speed = max(60.0, base_speed * (1.0 + variance * 0.5))
            velocity = heading * (max_speed * 0.5)
            agent = Agent(
                ident=ident,
                position=position,
                velocity=velocity,
                max_speed=max_speed,
                detection_radius=self.detection_radius,
            )
            self.agents.append(agent)
        initial_it = self.rng.randrange(len(self.agents))
        self.agents[initial_it].state = "it"
        self.agents[initial_it].colour = (240, 90, 90)
        self.tag_order.append(self.agents[initial_it].ident)

    def _steer_around_obstacles(self, agent: Agent) -> Vector:
        steering = Vector()
        for rect in self.arena.obstacles:
            closest_x = max(rect.left, min(agent.position.x, rect.right))
            closest_y = max(rect.top, min(agent.position.y, rect.bottom))
            closest = Vector(closest_x, closest_y)
            diff = agent.position - closest
            dist = diff.length()
            if dist < 80:
                if dist > 0:
                    push = diff.normalize() * (80 - dist)
                    steering += push
                else:
                    steering += (
                        Vector(self.rng.uniform(-1, 1), self.rng.uniform(-1, 1)) * 40
                    )
        return steering

    def _update_agent(self, agent: Agent, dt: float) -> None:
        steering = Vector()
        if agent.state == "it":
            target = self._nearest_runner(agent)
            if target is not None:
                steering += agent.steer_seek(target.position)
            else:
                steering += agent.steer_wander()
        else:
            hunter = self._current_it()
            if hunter is not None:
                distance = agent.position.distance_to(hunter.position)
                if distance < agent.detection_radius * 1.2:
                    steering += agent.steer_flee(hunter.position) * 1.2
                else:
                    steering += agent.steer_wander()
            else:
                steering += agent.steer_wander()
        steering += self._steer_around_obstacles(agent) * 0.4
        # Add small jitter to prevent stagnation
        steering += Vector(self.rng.uniform(-10, 10), self.rng.uniform(-10, 10))

        agent.velocity += steering * dt
        speed = agent.velocity.length()
        if speed > agent.max_speed:
            agent.velocity.scale_to_length(agent.max_speed)
        agent.position += agent.velocity * dt
        agent.position = self.arena.wrap(agent.position)

    def _current_it(self) -> Optional[Agent]:
        for agent in self.agents:
            if agent.state == "it":
                return agent
        return None

    def _nearest_runner(self, hunter: Agent) -> Optional[Agent]:
        closest_agent = None
        closest_dist = float("inf")
        for agent in self.agents:
            if agent.state == "runner":
                distance = hunter.position.distance_to(agent.position)
                if distance < closest_dist and distance <= hunter.detection_radius:
                    closest_dist = distance
                    closest_agent = agent
        return closest_agent

    def _check_tags(self) -> None:
        hunter = self._current_it()
        if hunter is None:
            return
        for agent in self.agents:
            if agent is hunter:
                continue
            if (
                agent.state == "runner"
                and hunter.position.distance_to(agent.position) <= self.tag_distance
            ):
                agent.state = "it"
                agent.colour = (240, 90, 90)
                hunter.state = "runner"
                hunter.colour = (80, 200, 120)
                event = TagEvent(
                    time=self.time_elapsed,
                    step=self.step_count,
                    hunter=hunter.ident,
                    runner=agent.ident,
                )
                self.events.append(event)
                self.tag_order.append(agent.ident)
                hunter.tag_times.append(self.time_elapsed)
                break

    def step(self, dt: float, step_count: int) -> None:
        self.step_count = step_count
        for agent in self.agents:
            self._update_agent(agent, dt)
        self._check_tags()
        self.time_elapsed += dt

    def snapshot(self) -> Dict:
        return {
            "time": self.time_elapsed,
            "agents": [
                {
                    "id": agent.ident,
                    "x": agent.position.x,
                    "y": agent.position.y,
                    "vx": agent.velocity.x,
                    "vy": agent.velocity.y,
                    "state": agent.state,
                }
                for agent in self.agents
            ],
            "events": [event.__dict__ for event in self.events],
            "tag_order": self.tag_order,
        }


def draw_scene(
    screen: pygame.Surface, sim: TagSimulation, font: pygame.font.Font
) -> None:
    screen.fill((32, 36, 44))
    for rect in sim.arena.obstacles:
        pygame.draw.rect(screen, (90, 90, 120), rect)
    for agent in sim.agents:
        centre = (int(agent.position.x), int(agent.position.y))
        pygame.draw.circle(screen, agent.colour, centre, 10)
        pygame.draw.circle(
            screen, (255, 255, 255), centre, int(agent.detection_radius), width=1
        )
    metrics = font.render(
        f"Time {sim.time_elapsed:5.1f}s  Tags {len(sim.events)}", True, (240, 240, 240)
    )
    screen.blit(metrics, (16, 16))
    order = font.render(
        f"Tag order: {' → '.join(map(str, sim.tag_order))}", True, (240, 240, 240)
    )
    screen.blit(order, (16, 48))
    pygame.display.flip()


def _serialise_obstacles(arena: Arena) -> List[Tuple[int, int, int, int]]:
    return [(rect.x, rect.y, rect.width, rect.height) for rect in arena.obstacles]


def run_visual(
    sim: TagSimulation, duration: float, record_path: Optional[Path] = None
) -> Dict:
    clock = pygame.time.Clock()
    screen = pygame.display.set_mode(sim.arena.size)
    pygame.display.set_caption("Multi-Agent Tag Simulation")
    font = pygame.font.SysFont("fira code", 18)
    running = True
    frames: List[Dict] = []
    elapsed = 0.0
    step = 0
    while running and elapsed < duration:
        dt = clock.tick(60) / 1000.0
        elapsed += dt
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
        sim.step(dt, step)
        draw_scene(screen, sim, font)
        if record_path:
            frames.append(sim.snapshot())
        step += 1
    pygame.quit()
    payload = {
        "config": {
            "agent_count": sim.agent_count,
            "speed_variance": sim.speed_variance,
            "detection_radius": sim.detection_radius,
            "arena_size": sim.arena.size,
        },
        "events": [event.__dict__ for event in sim.events],
        "tag_order": sim.tag_order,
        "duration": sim.time_elapsed,
        "obstacles": _serialise_obstacles(sim.arena),
    }
    if record_path:
        payload["frames"] = frames
        record_path.write_text(json.dumps(payload, indent=2))
    return payload


def run_headless(
    sim: TagSimulation,
    duration: float,
    max_steps: Optional[int],
    record_path: Optional[Path],
) -> Dict:
    dt = 1.0 / 60.0
    frames: List[Dict] = []
    steps = 0
    while sim.time_elapsed < duration:
        sim.step(dt, steps)
        if record_path:
            frames.append(sim.snapshot())
        steps += 1
        if max_steps and steps >= max_steps:
            break
    payload = {
        "config": {
            "agent_count": sim.agent_count,
            "speed_variance": sim.speed_variance,
            "detection_radius": sim.detection_radius,
            "arena_size": sim.arena.size,
        },
        "events": [event.__dict__ for event in sim.events],
        "tag_order": sim.tag_order,
        "duration": sim.time_elapsed,
        "obstacles": _serialise_obstacles(sim.arena),
    }
    if record_path:
        payload["frames"] = frames
        record_path.write_text(json.dumps(payload, indent=2))
    return payload


def run_replay(replay_path: Path) -> None:
    data = json.loads(replay_path.read_text())
    frames = data.get("frames", [])
    if not frames:
        raise SystemExit(
            "Replay does not contain frame data. Re-run simulation with --record."
        )
    arena_size = tuple(data.get("config", {}).get("arena_size", (800, 600)))
    obstacle_data = data.get("obstacles", [])
    pygame.display.set_caption("Multi-Agent Tag Replay")
    screen = pygame.display.set_mode(arena_size)
    font = pygame.font.SysFont("fira code", 18)
    clock = pygame.time.Clock()
    idx = 0
    running = True
    while running and idx < len(frames):
        dt = clock.tick(60)
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
        frame = frames[idx]
        screen.fill((20, 24, 32))
        for ox, oy, ow, oh in obstacle_data:
            pygame.draw.rect(screen, (90, 90, 120), pygame.Rect(ox, oy, ow, oh))
        for agent in frame["agents"]:
            colour = (240, 90, 90) if agent["state"] == "it" else (80, 200, 120)
            centre = (int(agent["x"]), int(agent["y"]))
            pygame.draw.circle(screen, colour, centre, 10)
        metrics = font.render(
            f"t={frame['time']:.2f}s tags={len(data.get('events', []))} order={'→'.join(map(str, data.get('tag_order', [])))}",
            True,
            (240, 240, 240),
        )
        screen.blit(metrics, (16, 16))
        pygame.display.flip()
        idx += 1
    pygame.quit()


def parse_args(argv: Optional[Sequence[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Multi-agent tag with steering behaviours"
    )
    parser.add_argument(
        "--agent-count", type=int, default=10, help="Number of agents in the arena"
    )
    parser.add_argument(
        "--speed-variance",
        type=float,
        default=0.4,
        help="Variance applied to base speed (+/- expressed as proportion)",
    )
    parser.add_argument(
        "--detection-radius",
        type=float,
        default=140.0,
        help="Distance at which agents can see each other",
    )
    parser.add_argument(
        "--duration",
        type=float,
        default=60.0,
        help="Maximum simulation time in seconds",
    )
    parser.add_argument(
        "--steps",
        type=int,
        default=0,
        help="Limit steps for headless runs (0 = unlimited)",
    )
    parser.add_argument(
        "--obstacles",
        type=int,
        default=3,
        help="How many rectangular obstacles to spawn",
    )
    parser.add_argument("--arena-width", type=int, default=960)
    parser.add_argument("--arena-height", type=int, default=720)
    parser.add_argument(
        "--seed", type=int, default=None, help="Random seed for reproducibility"
    )
    parser.add_argument(
        "--record", type=Path, default=None, help="Path to JSON replay file to write"
    )
    parser.add_argument(
        "--replay", type=Path, default=None, help="Replay a previously recorded run"
    )
    parser.add_argument(
        "--headless", action="store_true", help="Run without opening a pygame window"
    )
    parser.add_argument(
        "--export-metrics",
        type=Path,
        default=None,
        help="Write summary metrics to JSON",
    )
    return parser.parse_args(argv)


def ensure_video_driver(headless: bool) -> None:
    if headless:
        os.environ.setdefault("SDL_VIDEODRIVER", "dummy")


def main(argv: Optional[Sequence[str]] = None) -> None:
    args = parse_args(argv)
    if args.replay:
        ensure_video_driver(False)
        pygame.init()
        pygame.font.init()
        run_replay(args.replay)
        return

    ensure_video_driver(args.headless)
    pygame.init()
    pygame.font.init()

    arena = Arena(
        (args.arena_width, args.arena_height),
        obstacle_count=args.obstacles,
        seed=args.seed,
    )
    sim = TagSimulation(
        arena=arena,
        agent_count=args.agent_count,
        speed_variance=args.speed_variance,
        detection_radius=args.detection_radius,
        seed=args.seed,
    )

    if args.headless:
        payload = run_headless(sim, args.duration, args.steps or None, args.record)
    else:
        payload = run_visual(sim, args.duration, args.record)

    metrics = {
        "duration": payload["duration"],
        "tag_order": payload["tag_order"],
        "tags": payload["events"],
        "config": payload["config"],
    }
    if metrics["tags"]:
        metrics["time_to_first_tag"] = metrics["tags"][0]["time"]
        metrics["average_time_between_tags"] = metrics["tags"][-1]["time"] / len(
            metrics["tags"]
        )
    else:
        metrics["time_to_first_tag"] = None
        metrics["average_time_between_tags"] = None
    if args.export_metrics:
        args.export_metrics.write_text(json.dumps(metrics, indent=2))
    if args.headless:
        print(json.dumps(metrics, indent=2))


if __name__ == "__main__":
    main()
