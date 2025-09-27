import { Graphics, RenderTexture, Texture } from 'pixi.js';
import { BulletPool } from './bullets.ts';
import type { BulletDefinition } from './types.ts';

const bulletTextureCache: Map<string, Texture> = new Map();

const createCircleTexture = (radius: number, color: number): Texture => {
  const key = `${radius}-${color}`;
  if (bulletTextureCache.has(key)) {
    return bulletTextureCache.get(key)!;
  }
  const graphic = new Graphics();
  graphic.circle(radius, radius, radius).fill(color);
  const texture = RenderTexture.create({ width: radius * 2, height: radius * 2 });
  graphic.render({ renderTexture: texture });
  bulletTextureCache.set(key, texture);
  return texture;
};

export const registerDefaultBullets = (pool: BulletPool): void => {
  const definitions: Record<string, BulletDefinition> = {
    needle: {
      texture: createCircleTexture(6, 0xff4444),
      speed: 180,
      hitbox: { radius: 6 },
      damage: 1,
      grazeScore: 5
    },
    spread: {
      texture: createCircleTexture(8, 0x44aaff),
      speed: 140,
      hitbox: { radius: 8 },
      damage: 1,
      grazeScore: 8
    },
    orb: {
      texture: createCircleTexture(12, 0xffaaff),
      speed: 110,
      hitbox: { radius: 12 },
      damage: 2,
      grazeScore: 10
    }
  };

  for (const [id, definition] of Object.entries(definitions)) {
    pool.register(id, definition);
  }
};
