import { loadStageFromFile } from '../src/engine/scripting.ts';

const STAGES = ['stages/stage1.json', 'stages/stage2.lua'];

async function main(): Promise<void> {
  for (const stagePath of STAGES) {
    try {
      const stage = await loadStageFromFile(stagePath);
      console.log(`Loaded stage ${stage.id}: ${stage.title} (${stage.difficulty})`);
      if (stage.patterns.length === 0) {
        throw new Error('Stage defines no patterns');
      }
      if (stage.waves.length === 0) {
        throw new Error('Stage defines no waves');
      }
      const sorted = [...stage.patterns].sort((a, b) => a.steps.length - b.steps.length);
      console.log(`  Patterns: ${sorted.map((p) => `${p.id}(${p.steps.length})`).join(', ')}`);
    } catch (error) {
      console.error(`Failed to load stage ${stagePath}`);
      console.error(error);
      process.exitCode = 1;
    }
  }
}

main();
