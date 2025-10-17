import './style.css';
import { createGame } from './game';

document.querySelector<HTMLDivElement>('#app')!.innerHTML = `
  <div class="ui-wrapper">
    <div class="canvas-wrapper">
      <canvas id="game-canvas" width="800" height="800"></canvas>
    </div>
    <aside class="sidebar">
      <div class="badge">Challenge #108 &bull; Pipe Dreams</div>
      <div class="stats">
        <div class="stat-card"><h2>Level</h2><span id="stat-level">1</span></div>
        <div class="stat-card"><h2>Score</h2><span id="stat-score">0</span></div>
        <div class="stat-card"><h2>Time</h2><span id="stat-time">0s</span></div>
        <div class="stat-card"><h2>Leaks</h2><span id="stat-leaks">0</span></div>
      </div>
      <div class="message" id="game-message">Build a leak-free path and reach the sink.</div>
      <div class="instructions">
        <p><strong>Drag</strong> a pipe from the palette onto the grid. Locked tiles mark the source and sink.</p>
        <p><strong>Rearrange</strong> by dragging a placed pipe back into position. Completing a leak-free route before time runs out awards bonus points.</p>
      </div>
      <div class="palette" id="pipe-palette"></div>
      <div class="actions">
        <button class="action" id="advance-level" disabled>Next Level</button>
        <button class="action" id="reset-level">Reset Level</button>
      </div>
    </aside>
  </div>
`;

const canvas = document.querySelector<HTMLCanvasElement>('#game-canvas');
const paletteContainer = document.querySelector<HTMLDivElement>('#pipe-palette');
const messageElement = document.querySelector<HTMLDivElement>('#game-message');
const stats = {
  level: document.querySelector<HTMLSpanElement>('#stat-level')!,
  score: document.querySelector<HTMLSpanElement>('#stat-score')!,
  time: document.querySelector<HTMLSpanElement>('#stat-time')!,
  leaks: document.querySelector<HTMLSpanElement>('#stat-leaks')!
};
const advanceButton = document.querySelector<HTMLButtonElement>('#advance-level');
const resetButton = document.querySelector<HTMLButtonElement>('#reset-level');

if (!canvas || !paletteContainer || !messageElement || !advanceButton || !resetButton) {
  throw new Error('Failed to initialise UI elements.');
}

createGame({ canvas, paletteContainer, messageElement, stats, advanceButton, resetButton });
