import { Container, Sprite, Texture } from 'pixi.js';

export type Difficulty = 'easy' | 'normal' | 'hard' | 'lunatic';

export interface Vector2 {
  x: number;
  y: number;
}

export interface Hitbox {
  radius: number;
  offset?: Vector2;
}

export interface GrazeBox {
  radius: number;
  offset?: Vector2;
}

export interface BulletDefinition {
  texture: Texture;
  speed: number;
  turnSpeed?: number;
  lifeTime?: number;
  scale?: number;
  hitbox: Hitbox;
  damage: number;
  grazeScore?: number;
}

export interface BulletInstance {
  sprite: Sprite;
  position: Vector2;
  velocity: Vector2;
  angularVelocity?: number;
  timeAlive: number;
  definition: BulletDefinition;
  ownerId: string;
}

export interface PlayerStats {
  lives: number;
  bombs: number;
  continues: number;
  power: number;
  graze: number;
  score: number;
}

export interface PlayerConfig {
  speed: number;
  slowSpeedMultiplier: number;
  bombCount: number;
  continueCount: number;
  hitbox: Hitbox;
  grazeBox: GrazeBox;
}

export interface PowerUpDefinition {
  id: string;
  type: 'power' | 'life' | 'bomb' | 'score';
  value: number;
  texture: Texture;
  hitbox: Hitbox;
}

export interface EnemyDefinition {
  id: string;
  health: number;
  hitbox: Hitbox;
  texture: Texture;
  scripts: string[];
  powerDrop?: number;
  bombDrop?: number;
  lifeDrop?: number;
}

export interface PatternParameter {
  name: string;
  default: number;
}

export type PatternAction =
  | { type: 'spawnRadial'; bullet: string; count: number; speed?: number; radius?: number; }
  | { type: 'spawnSpiral'; bullet: string; count: number; revolutions: number; }
  | { type: 'spawnAimed'; bullet: string; speed?: number; }
  | { type: 'wait'; duration: number; }
  | { type: 'playSound'; id: string };

export interface PatternStep {
  time: number;
  action: PatternAction;
}

export interface BulletPattern {
  id: string;
  parameters?: PatternParameter[];
  steps: PatternStep[];
}

export interface StageWave {
  time: number;
  enemies: string[];
  patternOverrides?: Record<string, Record<string, number>>;
}

export interface StageDefinition {
  id: string;
  title: string;
  difficulty: Difficulty;
  background: string;
  boss: string;
  waves: StageWave[];
  patterns: BulletPattern[];
  configOverrides?: Partial<GameConfig>;
}

export interface BossScriptEvent {
  time: number;
  pattern: string;
  parameters?: Record<string, number>;
}

export interface BossScript {
  id: string;
  events: BossScriptEvent[];
}

export interface GameConfig {
  width: number;
  height: number;
  backgroundColor: number;
  player: PlayerConfig;
  difficultyScaling: Record<Difficulty, number>;
}

export interface StageContext {
  container: Container;
  config: GameConfig;
  difficulty: Difficulty;
}
