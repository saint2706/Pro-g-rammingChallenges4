import { ParticleContainer, Sprite, Texture } from 'pixi.js';
import type { BulletDefinition, BulletInstance, Vector2 } from './types.ts';

interface BulletRegistryEntry {
  id: string;
  definition: BulletDefinition;
}

export class BulletPool {
  private container: ParticleContainer;
  private bullets: BulletInstance[] = [];
  private registry: Map<string, BulletRegistryEntry> = new Map();
  private bounds = { width: 1024, height: 768 };
  private cellSize: number;
  private spatialIndex: Map<string, BulletInstance[]> = new Map();

  constructor(
    maxSize = 2048,
    bounds?: { width: number; height: number },
    cellSize = 96
  ) {
    this.container = new ParticleContainer(maxSize, {
      vertices: false,
      position: true,
      rotation: true,
      tint: true,
      uvs: false,
      scale: true
    });
    if (bounds) {
      this.bounds = bounds;
    }
    this.cellSize = cellSize;
  }

  get view(): ParticleContainer {
    return this.container;
  }

  setBounds(bounds: { width: number; height: number }): void {
    this.bounds = bounds;
  }

  register(id: string, definition: BulletDefinition): void {
    this.registry.set(id, { id, definition });
  }

  spawn(id: string, position: Vector2, velocity: Vector2, ownerId: string, overrides?: Partial<BulletDefinition>): BulletInstance {
    const entry = this.registry.get(id);
    if (!entry) {
      throw new Error(`Bullet definition ${id} is not registered`);
    }

    const definition = { ...entry.definition, ...overrides };
    const sprite = new Sprite(definition.texture ?? Texture.WHITE);
    sprite.anchor.set(0.5, 0.5);
    sprite.scale.set(definition.scale ?? 0.75);
    sprite.position.set(position.x, position.y);

    const instance: BulletInstance = {
      sprite,
      position: { ...position },
      velocity: { ...velocity },
      angularVelocity: definition.turnSpeed,
      timeAlive: 0,
      definition,
      ownerId
    };

    this.container.addChild(sprite);
    this.bullets.push(instance);
    return instance;
  }

  update(delta: number): void {
    this.spatialIndex.clear();
    for (let i = this.bullets.length - 1; i >= 0; i -= 1) {
      const bullet = this.bullets[i];
      bullet.timeAlive += delta;

      bullet.position.x += bullet.velocity.x * delta;
      bullet.position.y += bullet.velocity.y * delta;

      if (bullet.angularVelocity) {
        const angle = Math.atan2(bullet.velocity.y, bullet.velocity.x) + bullet.angularVelocity * delta;
        const speed = Math.hypot(bullet.velocity.x, bullet.velocity.y);
        bullet.velocity.x = Math.cos(angle) * speed;
        bullet.velocity.y = Math.sin(angle) * speed;
      }

      bullet.sprite.position.copyFrom(bullet.position as any);
      if (bullet.definition.lifeTime && bullet.timeAlive > bullet.definition.lifeTime) {
        this.recycle(i);
        continue;
      }

      if (
        bullet.position.x < -64 ||
        bullet.position.x > this.bounds.width + 64 ||
        bullet.position.y < -64 ||
        bullet.position.y > this.bounds.height + 64
      ) {
        this.recycle(i);
        continue;
      }

      const key = this.bucketKey(bullet.position.x, bullet.position.y);
      let bucket = this.spatialIndex.get(key);
      if (!bucket) {
        bucket = [];
        this.spatialIndex.set(key, bucket);
      }
      bucket.push(bullet);
    }
  }

  clear(): void {
    for (let i = this.bullets.length - 1; i >= 0; i -= 1) {
      this.recycle(i);
    }
  }

  get activeBullets(): readonly BulletInstance[] {
    return this.bullets;
  }

  forEachNearby(
    position: Vector2,
    radius: number,
    callback: (bullet: BulletInstance) => void
  ): void {
    const minX = Math.floor((position.x - radius) / this.cellSize);
    const maxX = Math.floor((position.x + radius) / this.cellSize);
    const minY = Math.floor((position.y - radius) / this.cellSize);
    const maxY = Math.floor((position.y + radius) / this.cellSize);
    for (let x = minX; x <= maxX; x += 1) {
      for (let y = minY; y <= maxY; y += 1) {
        const bucket = this.spatialIndex.get(this.bucketKeyFromIndices(x, y));
        if (!bucket) continue;
        for (const bullet of bucket) {
          callback(bullet);
        }
      }
    }
  }

  private recycle(index: number): void {
    const [bullet] = this.bullets.splice(index, 1);
    bullet.sprite.destroy();
  }

  private bucketKey(x: number, y: number): string {
    const ix = Math.floor(x / this.cellSize);
    const iy = Math.floor(y / this.cellSize);
    return this.bucketKeyFromIndices(ix, iy);
  }

  private bucketKeyFromIndices(ix: number, iy: number): string {
    return `${ix},${iy}`;
  }
}
