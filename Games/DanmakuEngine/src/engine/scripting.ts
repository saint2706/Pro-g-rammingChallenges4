import type { BossScript, StageDefinition } from './types.ts';

const isBrowser = typeof window !== 'undefined';

export async function loadStageFromFile(stagePath: string): Promise<StageDefinition> {
  const ext = stagePath.slice(stagePath.lastIndexOf('.'));
  if (ext === '.json') {
    const raw = await readStageFile(stagePath);
    return JSON.parse(raw) as StageDefinition;
  }
  if (ext === '.lua') {
    const code = await readStageFile(stagePath);
    return parseLuaStage(code);
  }
  throw new Error(`Unsupported stage file extension: ${ext}`);
}

export async function loadBossScripts(stageDir: string): Promise<BossScript[]> {
  try {
    const raw = await readStageFile(`${stageDir}/bossScripts.json`);
    return JSON.parse(raw) as BossScript[];
  } catch (error) {
    return [];
  }
}

async function readStageFile(stagePath: string): Promise<string> {
  if (isBrowser && typeof fetch !== 'undefined') {
    const url = new URL(stagePath, import.meta.url);
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`Failed to fetch ${stagePath}: ${response.statusText}`);
    }
    return response.text();
  }
  const { readFile } = await import('node:fs/promises');
  const path = await import('node:path');
  const { fileURLToPath } = await import('node:url');
  const filename = fileURLToPath(import.meta.url);
  const dirname = path.default.dirname(filename);
  const absolute = path.default.isAbsolute(stagePath)
    ? stagePath
    : path.default.join(dirname, '..', '..', stagePath);
  return readFile(absolute, 'utf8');
}

function parseLuaStage(luaSource: string): StageDefinition {
  const matcher = /\[\[([\s\S]*?)\]\]/g;
  let match: RegExpExecArray | null = null;
  let lastJsonBlock: string | null = null;
  while ((match = matcher.exec(luaSource)) !== null) {
    lastJsonBlock = match[1];
  }
  if (!lastJsonBlock) {
    throw new Error('Lua stage scripts must return a JSON string wrapped in [[ ... ]]');
  }
  return JSON.parse(lastJsonBlock);
}
