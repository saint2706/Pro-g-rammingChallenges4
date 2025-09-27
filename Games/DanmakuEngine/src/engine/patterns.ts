import { BulletPool } from './bullets.ts';
import type { BulletPattern, PatternAction, PatternStep, StageContext, Vector2 } from './types.ts';

const toRadians = (degrees: number): number => (degrees * Math.PI) / 180;

const vectorFromAngle = (angle: number, speed: number): Vector2 => ({
  x: Math.cos(angle) * speed,
  y: Math.sin(angle) * speed
});

export class PatternRunner {
  private elapsed = 0;
  private stepIndex = 0;

  constructor(
    private readonly pattern: BulletPattern,
    private readonly bullets: BulletPool,
    private readonly originProvider: () => Vector2,
    private readonly context: StageContext,
    private readonly parameterOverrides: Record<string, number> = {}
  ) {}

  update(delta: number): boolean {
    this.elapsed += delta;
    while (this.stepIndex < this.pattern.steps.length) {
      const step = this.pattern.steps[this.stepIndex];
      if (this.elapsed < step.time) {
        break;
      }
      this.execute(step.action);
      this.stepIndex += 1;
    }
    return this.stepIndex >= this.pattern.steps.length;
  }

  private resolveParameter(name: string, fallback: number): number {
    if (name in this.parameterOverrides) {
      return this.parameterOverrides[name];
    }
    const param = this.pattern.parameters?.find((p) => p.name === name);
    return param?.default ?? fallback;
  }

  private execute(action: PatternAction): void {
    switch (action.type) {
      case 'spawnRadial':
        this.spawnRadial(action);
        break;
      case 'spawnSpiral':
        this.spawnSpiral(action);
        break;
      case 'spawnAimed':
        this.spawnAimed(action);
        break;
      case 'wait':
        // waits are handled by schedule, nothing to do
        break;
      case 'playSound':
        // sound stub, integrate audio pipeline here
        break;
      default:
        break;
    }
  }

  private spawnRadial(action: Extract<PatternAction, { type: 'spawnRadial' }>): void {
    const origin = this.originProvider();
    const count = action.count;
    const baseSpeed = action.speed ?? this.resolveParameter('speed', 120);
    for (let i = 0; i < count; i += 1) {
      const angle = toRadians((360 / count) * i);
      const velocity = vectorFromAngle(angle, baseSpeed * this.scalingFactor());
      this.bullets.spawn(action.bullet, origin, velocity, this.pattern.id, {
        speed: baseSpeed
      });
    }
  }

  private spawnSpiral(action: Extract<PatternAction, { type: 'spawnSpiral' }>): void {
    const origin = this.originProvider();
    const count = action.count;
    const revolutions = action.revolutions;
    const baseSpeed = this.resolveParameter('speed', 90);
    const spiralDirection = this.resolveParameter('spiralDirection', 1);
    for (let i = 0; i < count; i += 1) {
      const t = i / count;
      const angle = toRadians(360 * revolutions * t * spiralDirection);
      const speed = baseSpeed * (1 + t * 0.5) * this.scalingFactor();
      const velocity = vectorFromAngle(angle, speed);
      this.bullets.spawn(action.bullet, origin, velocity, this.pattern.id);
    }
  }

  private spawnAimed(action: Extract<PatternAction, { type: 'spawnAimed' }>): void {
    const origin = this.originProvider();
    const player = this.context.container.getChildByName('player') as any as { position: Vector2 } | undefined;
    if (!player) {
      return;
    }
    const dx = player.position.x - origin.x;
    const dy = player.position.y - origin.y;
    const angle = Math.atan2(dy, dx);
    const speed = (action.speed ?? this.resolveParameter('speed', 140)) * this.scalingFactor();
    const velocity = vectorFromAngle(angle, speed);
    this.bullets.spawn(action.bullet, origin, velocity, this.pattern.id);
  }

  private scalingFactor(): number {
    return this.context.config.difficultyScaling[this.context.difficulty];
  }
}

export const createPatternTimeline = (
  pattern: BulletPattern,
  bullets: BulletPool,
  originProvider: () => Vector2,
  context: StageContext,
  overrides?: Record<string, number>
): PatternRunner => new PatternRunner(pattern, bullets, originProvider, context, overrides);

export const sortPatternSteps = (pattern: BulletPattern): BulletPattern => ({
  ...pattern,
  steps: [...pattern.steps].sort((a, b) => a.time - b.time)
});
