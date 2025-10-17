# Node Terminal Plotter

A Node.js implementation that renders graphs as ASCII art directly in the terminal window.

## Usage

```bash
npm install
node index.js "sin(x)" --xmin -6.28 --xmax 6.28
```

Pass any [mathjs](https://mathjs.org/docs/expressions/parsing.html) compatible expression in terms of `x`. Optional flags control the sampling window and number of rows used for rendering.
