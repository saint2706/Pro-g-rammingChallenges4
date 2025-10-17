#!/usr/bin/env node
import fs from 'fs';
import path from 'path';
import readline from 'readline';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const historyPath = path.join(__dirname, 'history.json');
const defaultRules = {
  hello: ["Hey there!", "Hello from Node.js", "Hi!"],
  "how are you": ["I'm running on V8 and feeling speedy!", "All systems go."],
  name: ["I'm a JavaScript chatbot.", "People call me Node-bot."],
  bye: ["Goodbye!", "Catch you later!"],
  help: ["Try greetings like 'hello', ask 'how are you', or say 'bye'."],
  default: [
    "Hmm, I'm not sure about that.",
    "Could you try rephrasing?",
    "I'm more of a small-talk bot right now."
  ]
};

function loadHistory() {
  try {
    const raw = fs.readFileSync(historyPath, 'utf-8');
    const data = JSON.parse(raw);
    if (Array.isArray(data)) {
      return data.map((entry) => ({
        user: String(entry.user ?? ''),
        bot: String(entry.bot ?? '')
      }));
    }
  } catch (err) {
    // Ignore errors and start with an empty history
  }
  return [];
}

function saveHistory(history) {
  try {
    fs.writeFileSync(historyPath, JSON.stringify(history, null, 2));
  } catch (err) {
    console.warn('Unable to persist chat history:', err.message);
  }
}

function matchTrigger(text) {
  const normalized = text.toLowerCase().trim();
  if (!normalized) {
    return 'default';
  }
  const tokens = normalized.split(/\s+/);
  const triggers = Object.keys(defaultRules).filter((key) => key !== 'default');
  for (const trig of triggers.filter((key) => key.includes(' '))) {
    const parts = trig.split(' ');
    if (parts.every((part) => tokens.includes(part))) {
      return trig;
    }
  }
  for (const trig of triggers) {
    if (normalized.includes(trig)) {
      return trig;
    }
  }
  return 'default';
}

let history = loadHistory();
let lastResponse = history.length ? history[history.length - 1].bot : null;

function getResponse(message) {
  const trigger = matchTrigger(message);
  const options = defaultRules[trigger] ?? defaultRules.default;
  if (!Array.isArray(options) || options.length === 0) {
    return '...';
  }
  const candidates = options.length > 1 && lastResponse
    ? options.filter((option) => option !== lastResponse)
    : options;
  const selected = candidates[Math.floor(Math.random() * candidates.length)];
  lastResponse = selected;
  history.push({ user: message, bot: selected });
  if (history.length > 100) {
    history = history.slice(-100);
  }
  saveHistory(history);
  return selected;
}

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

console.log('Node CLI chatbot ready! Type "exit" to quit.');

function loop() {
  rl.question('You: ', (input) => {
    const trimmed = input.trim();
    if (!trimmed) {
      console.log('Bot: Please say something!');
      return loop();
    }
    if (trimmed.toLowerCase() === 'exit' || trimmed.toLowerCase() === 'bye') {
      console.log('Bot: Bye!');
      rl.close();
      return;
    }
    const response = getResponse(trimmed);
    console.log(`Bot: ${response}`);
    loop();
  });
}

loop();
