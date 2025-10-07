#!/usr/bin/env node
import { Command } from 'commander';
import { create, all } from 'mathjs';

const math = create(all, {});

const program = new Command();
program
  .argument('<expression>', 'math expression in terms of x')
  .option('--xmin <number>', 'minimum x value', parseFloat, -10)
  .option('--xmax <number>', 'maximum x value', parseFloat, 10)
  .option('--columns <number>', 'samples across the x axis', parseInt, 80)
  .option('--rows <number>', 'rows to render', parseInt, 24)
  .description('Render a graph of the expression in the terminal.');

program.parse(process.argv);

const { xmin, xmax, columns, rows } = program.opts();
const expression = program.args[0];

if (!(xmax > xmin)) {
  console.error('--xmax must be greater than --xmin');
  process.exit(1);
}

const compiled = math.compile(expression);
const xs = Array.from({ length: columns }, (_, i) => xmin + ((xmax - xmin) * i) / (columns - 1));
const ys = xs.map((x) => {
  try {
    const result = compiled.evaluate({ x });
    return Number.isFinite(result) ? result : NaN;
  } catch (error) {
    console.error(`Failed to evaluate expression at x=${x}:`, error.message);
    process.exit(1);
  }
});

const finiteYs = ys.filter((y) => Number.isFinite(y));
if (finiteYs.length === 0) {
  console.error('No finite values produced for the given range.');
  process.exit(1);
}

const ymin = Math.min(...finiteYs);
const ymax = Math.max(...finiteYs);
const height = rows;
const width = columns;

const canvas = Array.from({ length: height }, () => Array.from({ length: width }, () => ' '));

const toRow = (y) => {
  if (ymax === ymin) {
    return Math.floor(height / 2);
  }
  const ratio = (y - ymin) / (ymax - ymin);
  return Math.max(0, Math.min(height - 1, height - 1 - Math.round(ratio * (height - 1))));
};

const zeroRow = 0 >= ymin && 0 <= ymax ? toRow(0) : null;
const zeroCol = 0 >= xmin && 0 <= xmax ? Math.round(((0 - xmin) / (xmax - xmin)) * (width - 1)) : null;

xs.forEach((_, idx) => {
  const y = ys[idx];
  if (!Number.isFinite(y)) {
    return;
  }
  const row = toRow(y);
  canvas[row][idx] = '•';
});

if (zeroRow !== null) {
  for (let col = 0; col < width; col += 1) {
    if (canvas[zeroRow][col] === '•') continue;
    canvas[zeroRow][col] = '-';
  }
}

if (zeroCol !== null) {
  for (let row = 0; row < height; row += 1) {
    if (canvas[row][zeroCol] === '•') continue;
    canvas[row][zeroCol] = '|';
  }
}

if (zeroRow !== null && zeroCol !== null) {
  canvas[zeroRow][zeroCol] = '+';
}

console.log(`f(x) = ${expression}`);
console.log(`x in [${xmin}, ${xmax}]`);
console.log(canvas.map((row) => row.join('')).join('\n'));
