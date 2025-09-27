import { Direction, DragPayload, FlowResult, LevelConfig, PipeTile, PipeType, Position, PuzzleDefinition } from './types';

const directionVectors: Record<Direction, Position> = {
  up: { row: -1, col: 0 },
  right: { row: 0, col: 1 },
  down: { row: 1, col: 0 },
  left: { row: 0, col: -1 }
};

const baseConnections: Record<PipeType, Direction[]> = {
  empty: [],
  source: ['right'],
  sink: ['left'],
  straight: ['up', 'down'],
  corner: ['up', 'right'],
  tee: ['up', 'left', 'right'],
  cross: ['up', 'right', 'down', 'left']
};

let tileIdCounter = 0;

export function nextTileId(): number {
  tileIdCounter += 1;
  return tileIdCounter;
}

export function cloneTile(tile: PipeTile): PipeTile {
  return { ...tile, id: nextTileId() };
}

export function createEmptyTile(): PipeTile {
  return { id: nextTileId(), type: 'empty', rotation: 0 };
}

export function rotateDirection(direction: Direction, rotation: number): Direction {
  const directions: Direction[] = ['up', 'right', 'down', 'left'];
  const index = directions.indexOf(direction);
  const steps = ((rotation / 90) % 4 + 4) % 4;
  return directions[(index + steps) % 4];
}

export function rotateDirections(directions: Direction[], rotation: number): Direction[] {
  return directions.map((direction) => rotateDirection(direction, rotation));
}

export function pipeConnections(tile: PipeTile): Direction[] {
  const base = baseConnections[tile.type];
  if (!base || base.length === 0) {
    return [];
  }
  return rotateDirections(base, tile.rotation);
}

export function opposite(direction: Direction): Direction {
  const opposites: Record<Direction, Direction> = {
    up: 'down',
    right: 'left',
    down: 'up',
    left: 'right'
  };
  return opposites[direction];
}

function tileKey(position: Position): string {
  return `${position.row},${position.col}`;
}

function randomInt(max: number): number {
  return Math.floor(Math.random() * max);
}

function shuffle<T>(items: T[]): T[] {
  const clone = [...items];
  for (let i = clone.length - 1; i > 0; i -= 1) {
    const j = Math.floor(Math.random() * (i + 1));
    [clone[i], clone[j]] = [clone[j], clone[i]];
  }
  return clone;
}

function inBounds(position: Position, rows: number, cols: number): boolean {
  return position.row >= 0 && position.row < rows && position.col >= 0 && position.col < cols;
}

function pickLevelConfig(level: number): LevelConfig {
  const rows = Math.min(6 + Math.floor(level / 2), 10);
  const cols = Math.min(6 + Math.ceil(level / 2), 12);
  const timeLimit = Math.max(60, 150 - level * 12);
  const paletteSize = Math.min(rows * cols, Math.floor(rows * cols * 0.45) + level + 4);
  return { rows, cols, timeLimit, paletteSize };
}

function buildPath(rows: number, cols: number, start: Position, sink: Position): Position[] {
  const result: Position[] = [];
  const visited = new Set<string>();

  const dfs = (position: Position): boolean => {
    result.push(position);
    if (position.row === sink.row && position.col === sink.col) {
      return true;
    }

    visited.add(tileKey(position));

    const neighbors = shuffle<Position>([
      { row: position.row - 1, col: position.col },
      { row: position.row + 1, col: position.col },
      { row: position.row, col: position.col - 1 },
      { row: position.row, col: position.col + 1 }
    ]).filter((neighbor) => inBounds(neighbor, rows, cols) && !visited.has(tileKey(neighbor)));

    neighbors.sort((a, b) => {
      const aDistance = Math.abs(a.row - sink.row) + Math.abs(a.col - sink.col);
      const bDistance = Math.abs(b.row - sink.row) + Math.abs(b.col - sink.col);
      return aDistance - bDistance + (Math.random() - 0.5);
    });

    for (const neighbor of neighbors) {
      if (dfs(neighbor)) {
        return true;
      }
    }

    result.pop();
    visited.delete(tileKey(position));
    return false;
  };

  dfs(start);
  return result;
}

function determineType(connections: Direction[]): PipeType {
  const sorted = [...connections].sort();
  const signature = sorted.join(',');
  switch (signature) {
    case 'down,up':
    case 'left,right':
      return 'straight';
    case 'down,right':
    case 'down,left':
    case 'left,up':
    case 'right,up':
      return 'corner';
    case 'down,left,right':
    case 'left,right,up':
    case 'down,left,up':
    case 'down,right,up':
      return 'tee';
    case 'down,left,right,up':
      return 'cross';
    default:
      return 'empty';
  }
}

function matchRotation(type: PipeType, connections: Direction[]): number {
  const base = baseConnections[type];
  if (!base || base.length === 0) {
    return 0;
  }

  for (const rotation of [0, 90, 180, 270]) {
    const rotated = rotateDirections(base, rotation);
    if (rotated.length === connections.length) {
      const same = rotated.every((direction) => connections.includes(direction));
      if (same) {
        return rotation;
      }
    }
  }
  return 0;
}

function composeTile(type: PipeType, rotation: number, locked = false): PipeTile {
  return {
    id: nextTileId(),
    type,
    rotation,
    locked
  };
}

export function generatePuzzle(level: number): PuzzleDefinition {
  const levelConfig = pickLevelConfig(level);
  const { rows, cols } = levelConfig;
  const board: PipeTile[][] = Array.from({ length: rows }, () => Array.from({ length: cols }, () => createEmptyTile()));

  const source: Position = { row: randomInt(rows), col: 0 };
  const sink: Position = { row: randomInt(rows), col: cols - 1 };

  const path = buildPath(rows, cols, source, sink);

  const solution = new Map<string, PipeTile>();
  const palettePieces: PipeTile[] = [];

  path.forEach((position, index) => {
    const key = tileKey(position);
    if (index === 0) {
      const next = path[index + 1];
      const direction = next ? determineDirection(position, next) : 'right';
      const rotation = matchRotation('source', [direction]);
      const tile = composeTile('source', rotation, true);
      board[position.row][position.col] = tile;
      solution.set(key, cloneTile(tile));
    } else if (index === path.length - 1) {
      const previous = path[index - 1];
      const direction = previous ? determineDirection(position, previous) : 'left';
      const rotation = matchRotation('sink', [direction]);
      const tile = composeTile('sink', rotation, true);
      board[position.row][position.col] = tile;
      solution.set(key, cloneTile(tile));
    } else {
      const previous = path[index - 1];
      const next = path[index + 1];
      const connections = [determineDirection(position, previous), determineDirection(position, next)];
      const type = determineType(connections);
      const rotation = matchRotation(type, connections);
      const tile = composeTile(type, rotation, false);
      solution.set(key, cloneTile(tile));
      palettePieces.push(cloneTile(tile));
    }
  });

  const extraPieces = Math.max(2, Math.min(levelConfig.paletteSize - palettePieces.length, level + 4));
  const mix: PipeType[] = ['straight', 'corner', 'tee'];
  for (let i = 0; i < extraPieces; i += 1) {
    const type = mix[randomInt(mix.length)];
    const rotation = [0, 90, 180, 270][randomInt(4)];
    palettePieces.push(composeTile(type, rotation));
  }

  const palette = shuffle(palettePieces).map((tile) => ({ ...tile, id: nextTileId() }));

  return { board, palette, source, sink, solution, levelConfig };
}

function determineDirection(from: Position, to: Position): Direction {
  if (to.row === from.row) {
    if (to.col > from.col) {
      return 'right';
    }
    return 'left';
  }
  if (to.row > from.row) {
    return 'down';
  }
  return 'up';
}

export function simulateFlow(board: PipeTile[][], source: Position): FlowResult {
  const visited = new Set<string>();
  const leaking: Position[] = [];
  const queue: Position[] = [source];
  const seen = new Set<string>([tileKey(source)]);

  while (queue.length > 0) {
    const current = queue.shift()!;
    const currentTile = board[current.row][current.col];
    const connections = pipeConnections(currentTile);

    visited.add(tileKey(current));

    connections.forEach((direction) => {
      const delta = directionVectors[direction];
      const next: Position = { row: current.row + delta.row, col: current.col + delta.col };
      if (!inBounds(next, board.length, board[0].length)) {
        leaking.push(current);
        return;
      }
      const neighborTile = board[next.row][next.col];
      if (neighborTile.type === 'empty') {
        leaking.push(next);
        return;
      }
      const neighborConnections = pipeConnections(neighborTile);
      if (!neighborConnections.includes(opposite(direction))) {
        leaking.push(next);
        return;
      }
      const key = tileKey(next);
      if (!seen.has(key)) {
        seen.add(key);
        queue.push(next);
      }
    });
  }

  const sinkReached = Array.from(visited).some((key) => {
    const [row, col] = key.split(',').map(Number);
    return board[row][col].type === 'sink';
  });

  return { reachedSink: sinkReached && leaking.length === 0, leaking, visited };
}

export function applyDrag(board: PipeTile[][], position: Position, payload: DragPayload | null): boolean {
  if (!payload) {
    return false;
  }
  const target = board[position.row][position.col];
  if (target.locked) {
    return false;
  }
  board[position.row][position.col] = { ...payload.tile, id: nextTileId(), locked: false };
  return true;
}

export function removeTile(board: PipeTile[][], position: Position): PipeTile | null {
  const tile = board[position.row][position.col];
  if (tile.locked) {
    return null;
  }
  board[position.row][position.col] = createEmptyTile();
  return tile;
}

export function resetBoard(board: PipeTile[][], puzzle: PuzzleDefinition): void {
  for (let row = 0; row < board.length; row += 1) {
    for (let col = 0; col < board[row].length; col += 1) {
      const key = tileKey({ row, col });
      const solutionTile = puzzle.solution.get(key);
      if (solutionTile && solutionTile.locked) {
        board[row][col] = cloneTile(solutionTile);
      } else {
        board[row][col] = createEmptyTile();
      }
    }
  }
}

export function highlightLeaks(board: PipeTile[][], leaks: Position[]): void {
  for (let row = 0; row < board.length; row += 1) {
    for (let col = 0; col < board[row].length; col += 1) {
      board[row][col].highlighted = false;
    }
  }
  leaks.forEach((position) => {
    if (inBounds(position, board.length, board[0].length)) {
      board[position.row][position.col].highlighted = true;
    }
  });
}
