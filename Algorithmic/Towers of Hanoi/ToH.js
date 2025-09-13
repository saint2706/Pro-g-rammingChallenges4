#!/usr/bin/env node
/**
 * ToH.js - Towers of Hanoi solver (recursive + iterative) with modern CLI.
 *
 * Features:
 *  - Recursive and iterative solvers
 *  - JSON output mode (--json)
 *  - Count-only mode without enumerating moves (--count-only)
 *  - Step limiting (--max-steps) for large n previews
 *  - Configurable peg labels (--pegs A B C)
 *  - Pure functions returning arrays (no side-effects) for embedding
 *
 * Usage examples:
 *   node ToH.js --disks 5
 *   node ToH.js -n 6 --iterative --max-steps 10 --json
 *   node ToH.js -n 4 --pegs X Z Y
 *   node ToH.js -n 10 --count-only --json
 */

// ------------------ Core Recursive Generator (Array Builder) ------------------
function recursiveMoves(n, from, to, aux, out) {
  if (n === 0) return;
  recursiveMoves(n - 1, from, aux, to, out);
  out.push(`Move disk ${n} from ${from} to ${to}`);
  recursiveMoves(n - 1, aux, to, from, out);
}

function towersOfHanoi(n, from, to, aux) {
  const moves = [];
  recursiveMoves(n, from, to, aux, moves);
  return moves;
}

// ------------------ Iterative Solver ------------------
function towersOfHanoiIterative(n, from, to, aux) {
  // Stack simulating recursion frames: [n, src, tgt, aux, stage]
  const stack = [[n, from, to, aux, 0]];
  const moves = [];
  while (stack.length) {
    const frame = stack.pop();
    if (!frame) break; // TS style guard (not strictly needed in JS)
    let [k, src, tgt, mid, stage] = frame;
    if (k === 0) continue;
    if (stage === 0) {
      stack.push([k, src, tgt, mid, 1]);
      stack.push([k - 1, src, mid, tgt, 0]);
    } else {
      moves.push(`Move disk ${k} from ${src} to ${tgt}`);
      stack.push([k - 1, mid, tgt, src, 0]);
    }
  }
  return moves;
}

// ------------------ CLI Parsing ------------------
function parseArgs(argv) {
  const args = {
    disks: 4,
    iterative: false,
    json: false,
    countOnly: false,
    maxSteps: null,
    pegs: ['A', 'B', 'C']
  };
  const it = argv[Symbol.iterator]();
  let cur = it.next();
  while (!cur.done) {
    const token = cur.value;
    if (token === '-n' || token === '--disks') {
      const nxt = it.next();
      if (nxt.done) throw new Error('Missing value for --disks');
      args.disks = parseInt(nxt.value, 10);
    } else if (token === '--iterative') {
      args.iterative = true;
    } else if (token === '--json') {
      args.json = true;
    } else if (token === '--count-only') {
      args.countOnly = true;
    } else if (token === '--max-steps') {
      const nxt = it.next();
      if (nxt.done) throw new Error('Missing value for --max-steps');
      args.maxSteps = parseInt(nxt.value, 10);
    } else if (token === '--pegs') {
      const p1 = it.next();
      const p2 = it.next();
      const p3 = it.next();
      if (p1.done || p2.done || p3.done) throw new Error('Need exactly three peg labels after --pegs');
      args.pegs = [p1.value, p2.value, p3.value];
    } else if (token === '--help' || token === '-h') {
      printHelp();
      process.exit(0);
    } else {
      // Ignore unknown tokens for forward compatibility
    }
    cur = it.next();
  }
  return args;
}

function printHelp() {
  console.log(`Towers of Hanoi Solver\n\n` +
    `Options:\n` +
    `  -n, --disks <int>      Number of disks (default 4)\n` +
    `  --iterative            Use iterative stack-based solver\n` +
    `  --json                 Emit JSON output\n` +
    `  --count-only           Only output move count\n` +
    `  --max-steps <int>      Limit number of moves emitted\n` +
    `  --pegs A B C           Custom peg labels (exactly 3)\n` +
    `  -h, --help             Show this help and exit\n`);
}

// ------------------ Main ------------------
function main() {
  const rawArgs = process.argv.slice(2);
  let cfg;
  try {
    cfg = parseArgs(rawArgs);
  } catch (e) {
    console.error(`Error: ${(e && e.message) || e}`);
    process.exit(1);
  }

  if (!Number.isInteger(cfg.disks) || cfg.disks < 1) {
    console.error('Error: disks must be a positive integer');
    process.exit(1);
  }
  if (cfg.maxSteps !== null && (!Number.isInteger(cfg.maxSteps) || cfg.maxSteps < 1)) {
    console.error('Error: max-steps must be a positive integer');
    process.exit(1);
  }
  if (cfg.pegs.length !== 3) {
    console.error('Error: exactly three peg labels required');
    process.exit(1);
  }

  const [fromPeg, toPeg, auxPeg] = cfg.pegs;
  const totalMoves = 2 ** cfg.disks - 1;

  let moves = [];
  if (!cfg.countOnly) {
    moves = (cfg.iterative ? towersOfHanoiIterative : towersOfHanoi)(cfg.disks, fromPeg, toPeg, auxPeg);
    if (cfg.maxSteps !== null) {
      moves = moves.slice(0, cfg.maxSteps);
    }
  }

  if (cfg.json) {
    const payload = {
      disks: cfg.disks,
      iterative: cfg.iterative,
      totalMoves,
      emittedMoves: moves.length,
      moves
    };
    console.log(JSON.stringify(payload, null, 2));
  } else {
    if (cfg.countOnly) {
      console.log(`Total moves: ${totalMoves}`);
    } else {
      moves.forEach((m, i) => console.log(`${i + 1}. ${m}`));
      console.log(`\nTotal moves required: ${totalMoves}`);
    }
  }
}

if (require.main === module) {
  main();
}

// Export functions for potential reuse/testing
module.exports = { towersOfHanoi, towersOfHanoiIterative };
