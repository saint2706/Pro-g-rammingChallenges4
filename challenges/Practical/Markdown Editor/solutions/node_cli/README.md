# Node CLI Markdown Converter

Convert Markdown files to HTML straight from the command line. This solution uses `marked` for parsing and offers options for writing to disk or printing to stdout.

## Commands

```bash
npm install
node index.js README.md --out converted.html
```

Omit `--out` to stream the HTML to standard output.
