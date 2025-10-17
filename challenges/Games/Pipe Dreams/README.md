# Pipe Dreams (Challenge #108)

A modernised take on the classic Pipe Dreams puzzle built with TypeScript, Vite, and HTML5 canvas. Drag and drop pipe segments, clear leaks, and race the clock as the board grows with each level. Game logic lives in modular TypeScript utilities to make future AI-assist features trivial to slot in.

![Pipe Dreams gameplay](./dist-preview.png)

## Features

- **Procedural puzzles** – every level generates a fresh solvable path between source and sink.
- **Drag-and-drop board building** – move tiles from the palette or rearrange pieces already on the grid.
- **Real-time leak detection** – canvas rendering highlights pressure losses so you can patch the route.
- **Timer and scoring** – finish quickly for bigger bonuses, but leaks and timeouts chip away at your total.
- **Level progression** – larger boards and expanded palettes keep the difficulty curve interesting.
- **Modular architecture** – board generation, simulation, and rendering are split into discrete modules for AI or analytics add-ons.

## Getting Started

> Requires Node.js 18+ (tested with 18.18 and 20.11).

```bash
npm install
npm run dev
```

### Development server

- Runs on <http://localhost:5173> (configurable in `vite.config.ts`).
- Hot-module reloading keeps canvas state while you iterate on UI tweaks or logic modules.

### Production build

```bash
npm run build
```

Outputs a static bundle under `dist/` that you can serve with any HTTP server:

```bash
npm install -g serve
serve dist
```

The repository already ships a compiled `dist/` directory for quick play-testing without building locally.

## Project Structure

```
challenges/Games/Pipe Dreams/
├── dist/                # Production build artefacts (pre-generated)
├── index.html           # Vite entry point
├── package.json         # npm scripts (dev/build) and deps
├── src/
│   ├── game.ts          # Canvas orchestration + UI glue
│   ├── logic/
│   │   ├── board.ts     # Puzzle generation, drag helpers, flow simulation
│   │   └── types.ts     # Shared TypeScript definitions
│   ├── main.ts          # Bootstrap + DOM wiring
│   └── style.css        # Neon-tinged UI styling
├── tsconfig*.json       # TypeScript settings
└── vite.config.ts       # Vite dev server config
```

## Gameplay Notes

- Locked tiles mark the **source** (cyan) and **sink** (orange). Everything else is up to you.
- Dragging a placed piece lifts it off the board so you can reposition it. Drop outside the grid to return it to the palette.
- Leaks glow red. Seal them to recover pressure and unlock the "Next Level" button.
- Clearing a level grants a bonus scaled by remaining time and the current level index. Expired timers reset the board with a score penalty.

## Extending the Game

- The `logic/board.ts` module exposes pure helpers for generating puzzles, simulating flow, and applying drag operations—ideal for future AI solvers or analytics overlays.
- Plug in new pipe types by extending `PipeType`, mapping base connections, and updating the rendering routine in `game.ts`.
- Hook telemetry or tutorials by listening to the `setMessage` calls inside `PipeDreamsGame`.

## Static Build

Open `dist/index.html` directly in a browser or serve the folder statically. The build was generated with:

```bash
npm install
npm run build
```

Enjoy routing digital pressure! PRs that add AI opponents, puzzle seeds, or accessibility upgrades are welcome.
