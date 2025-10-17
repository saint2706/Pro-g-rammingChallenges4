import { Container, Graphics } from 'pixi.js';
import { POWER_UPS } from './config.ts';
import { Player } from './player.ts';
import type { PowerUpDefinition, Vector2 } from './types.ts';

interface PowerUpInstance {
  definition: PowerUpDefinition;
  sprite: Graphics;
  velocity: Vector2;
}

export class PowerUpManager {
  private instances: PowerUpInstance[] = [];
  private bounds = { width: 960, height: 720 };

  constructor(private readonly container: Container, bounds?: { width: number; height: number }) {
    if (bounds) {
      this.bounds = bounds;
    }
  }

  setBounds(bounds: { width: number; height: number }): void {
    this.bounds = bounds;
  }

  spawn(id: string, position: Vector2): void {
    const definition = POWER_UPS[id];
    if (!definition) {
      throw new Error(`Power-up ${id} is not defined`);
    }
    const sprite = new Graphics();
    sprite.circle(0, 0, definition.hitbox.radius).fill(0xffff99);
    sprite.position.set(position.x, position.y);
    this.container.addChild(sprite);
    this.instances.push({ definition, sprite, velocity: { x: 0, y: 80 } });
  }

  update(delta: number, player: Player): void {
    for (let i = this.instances.length - 1; i >= 0; i -= 1) {
      const instance = this.instances[i];
      instance.sprite.y += instance.velocity.y * delta;
      if (instance.sprite.y > this.bounds.height + 32) {
        this.recycle(i);
        continue;
      }
      const distance = Math.hypot(instance.sprite.x - player.position.x, instance.sprite.y - player.position.y);
      if (distance <= instance.definition.hitbox.radius + player.serializeConfig().hitbox.radius) {
        this.applyToPlayer(instance.definition, player);
        this.recycle(i);
      }
    }
  }

  private applyToPlayer(definition: PowerUpDefinition, player: Player): void {
    switch (definition.type) {
      case 'power':
        player.collectPower(definition.value);
        break;
      case 'life':
        player.collectLife();
        break;
      case 'bomb':
        player.collectBomb();
        break;
      case 'score':
        player.addScore(definition.value);
        break;
      default:
        break;
    }
  }

  private recycle(index: number): void {
    const [instance] = this.instances.splice(index, 1);
    instance.sprite.destroy();
  }
}
