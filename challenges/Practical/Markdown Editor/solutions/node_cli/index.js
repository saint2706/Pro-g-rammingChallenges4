#!/usr/bin/env node
import { Command } from 'commander';
import { marked } from 'marked';
import fs from 'node:fs/promises';
import path from 'node:path';

const program = new Command();
program
  .argument('<file>', 'Markdown file to convert')
  .option('--out <file>', 'Write output HTML to a file instead of stdout')
  .option('--gfm', 'Enable GitHub flavored Markdown', true)
  .option('--no-gfm', 'Disable GitHub flavored Markdown')
  .description('Convert Markdown documents to HTML');

program.parse(process.argv);

const options = program.opts();
const [file] = program.args;

async function convert() {
  const inputPath = path.resolve(process.cwd(), file);
  const text = await fs.readFile(inputPath, 'utf8');
  marked.setOptions({ gfm: options.gfm });
  const html = marked.parse(text);
  if (options.out) {
    const outPath = path.resolve(process.cwd(), options.out);
    await fs.writeFile(outPath, html, 'utf8');
    console.log(`Wrote HTML output to ${outPath}`);
  } else {
    process.stdout.write(html);
  }
}

convert().catch((error) => {
  console.error(error.message);
  process.exit(1);
});
