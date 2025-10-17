import { Container } from 'pixi.js';
import { BulletPool } from './bullets.ts';
import { PatternRunner, createPatternTimeline, sortPatternSteps } from './patterns.ts';
import type { BossScript, GameConfig, StageContext, StageDefinition, StageWave } from './types.ts';

interface ActivePattern {
  runner: PatternRunner;
  elapsed: number;
}

export class StageRunner {
  private time = 0;
  private waves: StageWave[] = [];
  private activePatterns: ActivePattern[] = [];
  private bossScripts: BossScript[] = [];
  private waveIndex = 0;

  constructor(
    private readonly stage: StageDefinition,
    private readonly container: Container,
    private readonly bullets: BulletPool,
    private readonly config: GameConfig
  ) {
    this.waves = [...stage.waves].sort((a, b) => a.time - b.time);
  }

  attachBossScripts(scripts: BossScript[]): void {
    this.bossScripts = scripts;
  }

  update(delta: number): void {
    this.time += delta;
    this.spawnWaves();

    for (let i = this.activePatterns.length - 1; i >= 0; i -= 1) {
      const active = this.activePatterns[i];
      if (active.runner.update(delta)) {
        this.activePatterns.splice(i, 1);
      }
    }
  }

  startBossPhase(bossId: string): void {
    const script = this.bossScripts.find((s) => s.id === bossId);
    if (!script) {
      return;
    }
    for (const event of script.events) {
      setTimeout(() => {
        const pattern = this.stage.patterns.find((p) => p.id === event.pattern);
        if (!pattern) {
          return;
        }
        const runner = createPatternTimeline(
          sortPatternSteps(pattern),
          this.bullets,
          () => ({ x: this.config.width / 2, y: 128 }),
          { container: this.container, config: this.config, difficulty: this.stage.difficulty },
          event.parameters
        );
        this.activePatterns.push({ runner, elapsed: 0 });
      }, event.time * 1000);
    }
  }

  private spawnWaves(): void {
    while (this.waveIndex < this.waves.length) {
      const wave = this.waves[this.waveIndex];
      if (wave.time > this.time) {
        break;
      }
      this.launchWave(wave);
      this.waveIndex += 1;
    }
  }

  private launchWave(wave: StageWave): void {
    for (const patternId of wave.enemies) {
      const pattern = this.stage.patterns.find((p) => p.id === patternId);
      if (!pattern) {
        continue;
      }
      const runner = createPatternTimeline(
        sortPatternSteps(pattern),
        this.bullets,
        () => ({ x: this.config.width / 2, y: 200 }),
        { container: this.container, config: this.config, difficulty: this.stage.difficulty },
        wave.patternOverrides?.[patternId] as Record<string, number> | undefined
      );
      this.activePatterns.push({ runner, elapsed: 0 });
    }
  }
}
