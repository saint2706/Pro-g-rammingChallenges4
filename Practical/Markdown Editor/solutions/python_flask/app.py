from __future__ import annotations

from dataclasses import dataclass
from flask import Flask, jsonify, render_template_string, request
from markdown import markdown

app = Flask(__name__)


PAGE_TEMPLATE = """
<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <title>Flask Markdown Editor</title>
    <style>
      :root {
        color-scheme: light dark;
        font-family: system-ui, sans-serif;
      }
      body {
        margin: 0;
        display: grid;
        grid-template-columns: 1fr 1fr;
        height: 100vh;
      }
      textarea {
        width: 100%;
        height: 100%;
        border: none;
        padding: 1rem;
        font-size: 1rem;
        resize: none;
        box-sizing: border-box;
      }
      #preview {
        overflow: auto;
        padding: 1rem;
        border-left: 1px solid rgba(128, 128, 128, 0.3);
        background: rgba(240, 240, 240, 0.6);
      }
      @media (max-width: 800px) {
        body { grid-template-columns: 1fr; grid-template-rows: 50vh 50vh; }
      }
    </style>
  </head>
  <body>
    <textarea id="editor" placeholder="# Start writing...">{{ seed|e }}</textarea>
    <section id="preview">{{ rendered|safe }}</section>
    <script>
      const editor = document.getElementById('editor');
      const preview = document.getElementById('preview');
      let debounce;
      async function render() {
        const body = JSON.stringify({ content: editor.value });
        const res = await fetch('/preview', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body,
        });
        const data = await res.json();
        preview.innerHTML = data.html;
      }
      editor.addEventListener('input', () => {
        clearTimeout(debounce);
        debounce = setTimeout(render, 150);
      });
    </script>
  </body>
</html>
"""


@dataclass
class Preview:
    content: str

    @property
    def html(self) -> str:
        return markdown(self.content, extensions=["fenced_code", "tables"])


@app.post("/preview")
def live_preview() -> tuple[str, int] | tuple[dict[str, str], int]:
    payload = request.get_json(silent=True) or {}
    content = str(payload.get("content", ""))
    preview = Preview(content=content)
    return jsonify({"html": preview.html}), 200


@app.get("/")
def index() -> str:
    seed = "---\ntitle: Flask Markdown\n---\n\n# Welcome\n\nStart writing Markdown in the left pane."
    preview = Preview(content=seed)
    return render_template_string(PAGE_TEMPLATE, seed=seed, rendered=preview.html)


if __name__ == "__main__":
    app.run(debug=True)
