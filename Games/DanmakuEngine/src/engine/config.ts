import { Texture } from 'pixi.js';
import type { GameConfig, PowerUpDefinition } from './types.ts';

export const BASE_CONFIG: GameConfig = {
  width: 960,
  height: 720,
  backgroundColor: 0x030311,
  player: {
    speed: 320,
    slowSpeedMultiplier: 0.45,
    bombCount: 3,
    continueCount: 2,
    hitbox: { radius: 4, offset: { x: 0, y: 6 } },
    grazeBox: { radius: 18, offset: { x: 0, y: 6 } }
  },
  difficultyScaling: {
    easy: 0.75,
    normal: 1,
    hard: 1.25,
    lunatic: 1.5
  }
};

export const POWER_UPS: Record<string, PowerUpDefinition> = {
  powerSmall: {
    id: 'powerSmall',
    type: 'power',
    value: 1,
    texture: Texture.WHITE,
    hitbox: { radius: 8 }
  },
  powerLarge: {
    id: 'powerLarge',
    type: 'power',
    value: 5,
    texture: Texture.WHITE,
    hitbox: { radius: 10 }
  },
  bomb: {
    id: 'bomb',
    type: 'bomb',
    value: 1,
    texture: Texture.WHITE,
    hitbox: { radius: 12 }
  },
  extend: {
    id: 'extend',
    type: 'life',
    value: 1,
    texture: Texture.WHITE,
    hitbox: { radius: 12 }
  }
};
