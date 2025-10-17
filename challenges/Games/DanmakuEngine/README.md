# Danmaku Engine (TypeScript + PixiJS)

A reusable bullet-hell sandbox that layers PixiJS rendering, bullet pattern scripting, and boss timelines. The engine focuses on
danmaku staples—dense curtain fire, grazing, bombs, and continues—while keeping authoring approachable through declarative JSON
or Lua scripts.

![Danmaku Engine preview](../programming%20challenges.png)

## Feature Highlights

- **PixiJS rendering pipeline** with `ParticleContainer` sprite batching for thousands of bullets on screen.
- **Bullet systems** supporting radial, spiral, and aimed patterns driven by timeline steps.
- **Boss scripting**: attach pattern schedules to named bosses for multi-phase fights.
- **Precision hitboxes and graze detection**: separate damage and graze radii yield high-score play.
- **Power-up economy** including power, extends, bombs, and score shards.
- **Player systems** with configurable focus slowdown, bombs, and continues.
- **Reusable scripting**: author stages in JSON or Lua (exporting JSON strings) to mix rapid iteration with power users’ tooling.

## Getting Started

```bash
cd challenges/Games/DanmakuEngine
npm install
npm run dev
```

Open <http://localhost:5173> to see the demo stages.

### Stage validation

```bash
npm run stage:validate
```

This loads every stage script (JSON + Lua) to ensure they register waves/patterns correctly.

## Architecture Overview

```
src/
├── main.ts               # Bootstraps Pixi, player controller, and stage runner
└── engine/
    ├── bullets.ts        # ParticleContainer-backed bullet pool with pooling + lifecycle
    ├── collision.ts      # Hitbox + graze radius checks
    ├── config.ts         # Base game config and power-up definitions
    ├── entities.ts       # Procedural bullet textures + registry helpers
    ├── patterns.ts       # Timeline-driven pattern runners (radial, spiral, aimed)
    ├── player.ts         # Player movement, slowdown, bombs, continues, stat tracking
    ├── powerups.ts       # Power-up spawning/collection behaviour
    ├── scripting.ts      # JSON + Lua (JSON string bridge) stage loading
    └── stage.ts          # Stage progression, boss scripting hooks
```

### Rendering + batching

- Bullets are rendered through a Pixi `ParticleContainer`, enabling GPU-friendly batching even for thousands of sprites.
- Bullet textures are generated procedurally (`entities.ts`) so stages can swap palettes without shipping binary assets.
- Backgrounds and UI sit in their own containers, making it easy to add parallax or shader passes later.

### Player system

- `Player` exposes speed + `slowSpeedMultiplier` (focus mode) values loaded from `config.ts` or per-stage overrides.
- Bombs clear all active bullets and grant brief invulnerability; continues refill bombs/lives while halving power.
- Graze events add score + graze counts, unlocking difficulty scaling hooks.

### Bullet pattern scripting

Patterns are composed of timeline steps, each firing an action at `time` seconds into the pattern. Supported actions:

| Action        | Description |
|---------------|-------------|
| `spawnRadial` | Fire `count` bullets evenly spaced around the origin. Optional `speed` override. |
| `spawnSpiral` | Fire bullets along a spiral curve. Parameters: `count`, `revolutions`, optional `speed` + `spiralDirection`. |
| `spawnAimed`  | Fire towards the player with optional speed override. |
| `wait`        | Schedule delay between actions (handled via `time` offsets). |
| `playSound`   | Hook for soundboard integration. |

Parameters defined in the pattern (e.g., `speed`, `spiralDirection`) can be overridden per wave or boss event.

### Boss scripting

Boss scripts live in `stages/bossScripts.json` and link named bosses to timed pattern launches. Each event contains:

```json
{
  "time": 4.5,
  "pattern": "crossSpiral",
  "parameters": { "spiralDirection": 1 }
}
```

Stage runners attach these scripts automatically; call `stageRunner.startBossPhase(bossId)` during gameplay to queue phases.

## Scripting API

### JSON stages

`stages/stage1.json` demonstrates the format:

```json
{
  "id": "stage1",
  "title": "Scarlet Garden",
  "difficulty": "normal",
  "background": "nebula",
  "boss": "crimsonCountess",
  "patterns": [
    {
      "id": "openingFan",
      "parameters": [ { "name": "speed", "default": 140 } ],
      "steps": [
        { "time": 0.0, "action": { "type": "spawnRadial", "bullet": "needle", "count": 12 } }
      ]
    }
  ],
  "waves": [ { "time": 1.5, "enemies": ["openingFan"] } ]
}
```

- `patterns`: Define bullet patterns available to waves/bosses.
- `waves`: Schedule pattern usage during stage progression. Optional `patternOverrides` let you tweak pattern parameters without
  duplicating the pattern.
- `configOverrides`: Partial `GameConfig` patches (e.g., adjusting player slowdown per stage).

### Lua stages

`stages/stage2.lua` exports the same structure but wrapped in a Lua file that returns a JSON string. This lets you preprocess data
with Lua tooling before handing the result to the TypeScript runtime.

```lua
local stage = build_stage() -- your Lua helper
return json.encode(stage)   -- stage is converted to JSON and ingested by the loader
```

### Power-ups

Use `PowerUpManager.spawn(id, position)` from gameplay scripts to drop power, bomb, or extend items. Default items are defined in
`config.ts`. Add new entries there to make them globally available.

## Sample Stages

| Stage         | Difficulty | Highlights |
|---------------|------------|------------|
| `stage1.json` | Normal     | Intro density ramp, radial fans, aimed rain, demonstrates graze scoring. |
| `stage2.lua`  | Hard       | Heavier density, reverse spirals, pattern overrides, harder scaling for Lunatic mode. |

Run `npm run stage:validate` to make sure custom stages load before launching the game.

## Extending the Engine

- **New pattern types**: Add new action handlers to `patterns.ts` (e.g., bezier curves or laser sweeps) and register via pattern
  timelines.
- **Shaders**: Wrap the bullet `ParticleContainer` with Pixi filters for bloom or screen shake.
- **UI/HUD**: Connect `player.stats` to a HUD overlay via Pixi `BitmapText`.
- **Replays**: Log input states each frame and feed them into deterministic runs.

## Challenge Status

This implementation fulfils Challenge #120 from the `/g/` Games list using TypeScript + PixiJS, complete with scripting support,
boss phases, and sample stages.
