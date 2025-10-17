export type Direction = 'up' | 'right' | 'down' | 'left';

export type PipeType =
  | 'empty'
  | 'source'
  | 'sink'
  | 'straight'
  | 'corner'
  | 'tee'
  | 'cross';

export interface PipeTile {
  id: number;
  type: PipeType;
  rotation: number; // 0, 90, 180, 270 degrees
  locked?: boolean;
  highlighted?: boolean;
}

export interface Position {
  row: number;
  col: number;
}

export interface LevelConfig {
  rows: number;
  cols: number;
  timeLimit: number;
  paletteSize: number;
}

export interface PuzzleDefinition {
  board: PipeTile[][];
  palette: PipeTile[];
  source: Position;
  sink: Position;
  solution: Map<string, PipeTile>;
  levelConfig: LevelConfig;
}

export interface FlowResult {
  reachedSink: boolean;
  leaking: Position[];
  visited: Set<string>;
}

export interface DragPayload {
  tile: PipeTile;
  fromPalette: boolean;
  sourceIndex?: number;
  originCell?: Position;
}
