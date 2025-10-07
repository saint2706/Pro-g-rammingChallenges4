# Chatbot – Node.js CLI

This solution reimplements the lightweight rule-based chatbot in Node.js. It
persists chat history to `history.json` and mirrors the trigger matching logic
from the Python version.

## Usage

```bash
npm install
npm start
```

The script exposes a binary named `chatbot-node`, so you can also install it
globally (`npm install -g .`) and run `chatbot-node` from any directory.

Type `exit` (or `bye`) to terminate the conversation.
