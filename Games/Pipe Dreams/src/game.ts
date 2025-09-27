import {
  applyDrag,
  cloneTile,
  generatePuzzle,
  highlightLeaks,
  pipeConnections,
  removeTile,
  resetBoard,
  simulateFlow
} from './logic/board';
import { DragPayload, PipeTile, Position, PuzzleDefinition } from './logic/types';

interface StatsView {
  level: HTMLElement;
  score: HTMLElement;
  time: HTMLElement;
  leaks: HTMLElement;
}

interface GameOptions {
  canvas: HTMLCanvasElement;
  paletteContainer: HTMLElement;
  messageElement: HTMLElement;
  stats: StatsView;
  advanceButton: HTMLButtonElement;
  resetButton: HTMLButtonElement;
}

type StatusTone = 'info' | 'success' | 'warning';

const TILE_SIZE = 72;
const CANVAS_PADDING = 24;

export class PipeDreamsGame {
  private canvas: HTMLCanvasElement;

  private ctx: CanvasRenderingContext2D;

  private paletteContainer: HTMLElement;

  private messageElement: HTMLElement;

  private stats: StatsView;

  private advanceButton: HTMLButtonElement;

  private resetButton: HTMLButtonElement;

  private level = 1;

  private score = 0;

  private timeRemaining = 0;

  private board: PipeTile[][] = [];

  private palette: PipeTile[] = [];

  private puzzle: PuzzleDefinition | null = null;

  private dragPayload: DragPayload | null = null;

  private dragPosition = { x: 0, y: 0 };

  private lastTick = performance.now();

  private levelComplete = false;

  private leakCount = 0;

  private statusTone: StatusTone = 'info';

  constructor(options: GameOptions) {
    this.canvas = options.canvas;
    const context = this.canvas.getContext('2d');
    if (!context) {
      throw new Error('Canvas context unavailable');
    }
    this.ctx = context;
    this.paletteContainer = options.paletteContainer;
    this.messageElement = options.messageElement;
    this.stats = options.stats;
    this.advanceButton = options.advanceButton;
    this.resetButton = options.resetButton;

    this.canvas.addEventListener('pointerdown', (event) => this.onCanvasPointerDown(event));
    window.addEventListener('pointermove', (event) => this.onPointerMove(event));
    window.addEventListener('pointerup', (event) => this.onPointerUp(event));

    this.advanceButton.addEventListener('click', () => {
      if (this.levelComplete) {
        this.level += 1;
        this.startLevel();
      }
    });

    this.resetButton.addEventListener('click', () => {
      this.startLevel(true);
    });

    this.startLevel();
    requestAnimationFrame(() => this.tick());
  }

  private startLevel(force = false): void {
    if (this.levelComplete && !force) {
      return;
    }
    this.levelComplete = false;
    this.statusTone = 'info';
    this.setMessage(`Route water from source to sink. Use all the time you can to earn more points.`, 'info');
    this.advanceButton.disabled = true;

    this.puzzle = generatePuzzle(this.level);
    this.board = this.puzzle.board.map((row) => row.map((tile) => ({ ...tile })));
    this.palette = this.puzzle.palette.map((tile) => cloneTile(tile));
    this.timeRemaining = this.puzzle.levelConfig.timeLimit;
    this.lastTick = performance.now();

    this.resizeCanvas();
    this.renderPalette();
    this.updateStats();
  }

  private resizeCanvas(): void {
    if (!this.board.length) {
      return;
    }
    const rows = this.board.length;
    const cols = this.board[0].length;
    this.canvas.width = cols * TILE_SIZE + CANVAS_PADDING * 2;
    this.canvas.height = rows * TILE_SIZE + CANVAS_PADDING * 2;
  }

  private renderPalette(): void {
    this.paletteContainer.innerHTML = '';
    this.palette.forEach((tile, index) => {
      const wrapper = document.createElement('div');
      wrapper.className = 'palette-tile';
      wrapper.dataset.index = index.toString();
      const preview = document.createElement('canvas');
      preview.width = TILE_SIZE;
      preview.height = TILE_SIZE;
      wrapper.appendChild(preview);
      const ctx = preview.getContext('2d');
      if (ctx) {
        this.drawTile(ctx, tile, 0, 0, TILE_SIZE, true);
      }
      wrapper.addEventListener('pointerdown', (event) => {
        event.preventDefault();
        this.onPalettePointerDown(event, index);
      });
      this.paletteContainer.appendChild(wrapper);
    });
  }

  private onPalettePointerDown(event: PointerEvent, index: number): void {
    if (!this.palette[index]) {
      return;
    }
    const [tile] = this.palette.splice(index, 1);
    this.dragPayload = { tile, fromPalette: true, sourceIndex: index };
    this.updateDragPosition(event);
    this.renderPalette();
  }

  private onCanvasPointerDown(event: PointerEvent): void {
    if (!this.board.length) {
      return;
    }
    const cell = this.eventToCell(event);
    if (!cell) {
      return;
    }
    const tile = this.board[cell.row][cell.col];
    if (tile.locked || tile.type === 'empty') {
      return;
    }
    const removed = removeTile(this.board, cell);
    if (!removed) {
      return;
    }
    this.dragPayload = { tile: removed, fromPalette: false, originCell: cell };
    this.updateDragPosition(event);
  }

  private onPointerMove(event: PointerEvent): void {
    if (!this.dragPayload) {
      return;
    }
    this.updateDragPosition(event);
  }

  private onPointerUp(event: PointerEvent): void {
    if (!this.dragPayload) {
      return;
    }

    const payload = this.dragPayload;
    this.dragPayload = null;

    const cell = this.eventToCell(event);
    if (cell && this.board[cell.row][cell.col] && applyDrag(this.board, cell, payload)) {
      this.updateFlowState();
      return;
    }

    if (payload.fromPalette) {
      const index = payload.sourceIndex ?? this.palette.length;
      this.palette.splice(index, 0, payload.tile);
      this.renderPalette();
    } else if (payload.originCell) {
      const { originCell } = payload;
      if (this.board[originCell.row][originCell.col].type === 'empty') {
        this.board[originCell.row][originCell.col] = payload.tile;
      }
    }
  }

  private updateDragPosition(event: PointerEvent): void {
    const rect = this.canvas.getBoundingClientRect();
    this.dragPosition = {
      x: event.clientX - rect.left,
      y: event.clientY - rect.top
    };
  }

  private eventToCell(event: PointerEvent): Position | null {
    const rect = this.canvas.getBoundingClientRect();
    const x = event.clientX - rect.left - CANVAS_PADDING;
    const y = event.clientY - rect.top - CANVAS_PADDING;
    if (!this.board.length) {
      return null;
    }
    const row = Math.floor(y / TILE_SIZE);
    const col = Math.floor(x / TILE_SIZE);
    if (row < 0 || col < 0 || row >= this.board.length || col >= this.board[0].length) {
      return null;
    }
    return { row, col };
  }

  private tick(): void {
    const now = performance.now();
    const delta = (now - this.lastTick) / 1000;
    this.lastTick = now;

    if (!this.levelComplete) {
      this.timeRemaining -= delta;
      if (this.timeRemaining <= 0) {
        this.timeRemaining = 0;
        this.onTimeExpired();
      }
    }

    this.updateFlowState();
    this.draw();
    requestAnimationFrame(() => this.tick());
  }

  private draw(): void {
    const { width, height } = this.canvas;
    this.ctx.clearRect(0, 0, width, height);

    this.ctx.save();
    this.ctx.translate(CANVAS_PADDING, CANVAS_PADDING);

    if (this.board.length) {
      const rows = this.board.length;
      const cols = this.board[0].length;
      for (let row = 0; row < rows; row += 1) {
        for (let col = 0; col < cols; col += 1) {
          const tile = this.board[row][col];
          const x = col * TILE_SIZE;
          const y = row * TILE_SIZE;
          this.drawTile(this.ctx, tile, x, y, TILE_SIZE, false);
        }
      }
    }

    if (this.dragPayload) {
      this.ctx.save();
      this.ctx.globalAlpha = 0.85;
      this.drawTile(this.ctx, this.dragPayload.tile, this.dragPosition.x - TILE_SIZE / 2, this.dragPosition.y - TILE_SIZE / 2, TILE_SIZE, false);
      this.ctx.restore();
    }

    this.ctx.restore();
  }

  private drawTile(
    context: CanvasRenderingContext2D,
    tile: PipeTile,
    x: number,
    y: number,
    size: number,
    standalone: boolean
  ): void {
    context.save();
    context.translate(x, y);

    const padding = size * 0.1;
    context.fillStyle = tile.highlighted ? 'rgba(248,113,113,0.25)' : 'rgba(15,23,42,0.95)';
    context.strokeStyle = tile.locked ? '#38bdf8' : 'rgba(148, 163, 184, 0.35)';
    context.lineWidth = 2;
    context.beginPath();
    context.roundRect(1, 1, size - 2, size - 2, 12);
    context.fill();
    context.stroke();

    if (tile.type !== 'empty') {
      const connections = pipeConnections(tile);
      const centerX = size / 2;
      const centerY = size / 2;
      const pipeWidth = size * 0.22;

      context.lineCap = 'round';
      context.lineWidth = pipeWidth;
      context.strokeStyle = tile.locked ? '#38bdf8' : '#f8fafc';

      connections.forEach((direction) => {
        context.beginPath();
        context.moveTo(centerX, centerY);
        switch (direction) {
          case 'up':
            context.lineTo(centerX, padding);
            break;
          case 'right':
            context.lineTo(size - padding, centerY);
            break;
          case 'down':
            context.lineTo(centerX, size - padding);
            break;
          case 'left':
            context.lineTo(padding, centerY);
            break;
          default:
            break;
        }
        context.stroke();
      });

      context.beginPath();
      context.lineWidth = pipeWidth;
      context.strokeStyle = tile.type === 'source' ? '#22d3ee' : tile.type === 'sink' ? '#f97316' : '#f8fafc';
      context.moveTo(centerX, centerY);
      context.lineTo(centerX, centerY);
      context.stroke();
    }

    if (standalone) {
      context.strokeStyle = 'rgba(148,163,184,0.35)';
      context.lineWidth = 1.5;
      context.strokeRect(0, 0, size, size);
    }

    context.restore();
  }

  private updateFlowState(): void {
    if (!this.puzzle) {
      return;
    }
    const flow = simulateFlow(this.board, this.puzzle.source);
    highlightLeaks(this.board, flow.leaking);
    this.leakCount = flow.leaking.length;
    this.updateStats();

    if (flow.leaking.length > 0 && !this.levelComplete) {
      this.setMessage('Leaks detected! Patch the gaps to restore pressure.', 'warning');
    } else if (!this.levelComplete) {
      this.setMessage('Build a leak-free path and reach the sink.', 'info');
    }

    if (flow.reachedSink && !this.levelComplete) {
      this.onLevelComplete();
    }
  }

  private onLevelComplete(): void {
    this.levelComplete = true;
    this.advanceButton.disabled = false;
    this.statusTone = 'success';
    const bonus = Math.round(this.timeRemaining * 5 + this.level * 100);
    this.score += bonus;
    this.setMessage(`Flow stabilised! Level ${this.level} cleared (+${bonus} pts).`, 'success');
  }

  private onTimeExpired(): void {
    if (!this.puzzle) {
      return;
    }
    this.score = Math.max(0, this.score - 75);
    this.setMessage('Time expired! Board reset and score penalised.', 'warning');
    resetBoard(this.board, this.puzzle);
    this.palette = this.puzzle.palette.map((tile) => cloneTile(tile));
    this.renderPalette();
    this.timeRemaining = this.puzzle.levelConfig.timeLimit;
  }

  private updateStats(): void {
    this.stats.level.textContent = this.level.toString();
    this.stats.score.textContent = Math.max(0, Math.round(this.score)).toString();
    this.stats.time.textContent = `${Math.ceil(this.timeRemaining)}s`;
    this.stats.leaks.textContent = this.leakCount.toString();
  }

  private setMessage(message: string, tone: StatusTone): void {
    if (this.statusTone === tone && this.messageElement.textContent === message) {
      return;
    }
    this.statusTone = tone;
    this.messageElement.textContent = message;
    this.messageElement.style.color = tone === 'success' ? '#34d399' : tone === 'warning' ? '#f87171' : '#e2e8f0';
  }
}

export function createGame(options: GameOptions): PipeDreamsGame {
  return new PipeDreamsGame(options);
}
