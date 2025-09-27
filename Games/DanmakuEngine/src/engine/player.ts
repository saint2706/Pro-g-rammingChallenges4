import { Container, Graphics } from 'pixi.js';
import { BulletPool } from './bullets.ts';
import type { GameConfig, PlayerConfig, PlayerStats, Vector2 } from './types.ts';

export class Player {
  public readonly stats: PlayerStats;
  public readonly sprite: Graphics;
  public position: Vector2;
  public isSlowed = false;
  public invulnerableTimer = 0;
  public bombCooldown = 0;

  constructor(private readonly container: Container, private readonly config: GameConfig) {
    const playerConfig = config.player;
    this.sprite = new Graphics();
    this.sprite.circle(0, 0, 4).fill(0xffffff);
    this.sprite.circle(0, 0, playerConfig.hitbox.radius).stroke({ color: 0xff3366, width: 1, alpha: 0.4 });
    this.position = { x: config.width / 2, y: config.height - 80 };
    this.sprite.position.set(this.position.x, this.position.y);
    container.addChild(this.sprite);

    this.stats = {
      lives: 3,
      bombs: playerConfig.bombCount,
      continues: playerConfig.continueCount,
      power: 1,
      graze: 0,
      score: 0
    };
  }

  update(delta: number): void {
    if (this.invulnerableTimer > 0) {
      this.invulnerableTimer = Math.max(0, this.invulnerableTimer - delta);
      this.sprite.alpha = this.invulnerableTimer > 0 ? 0.5 + Math.sin(this.invulnerableTimer * 25) * 0.2 : 1;
    }
    if (this.bombCooldown > 0) {
      this.bombCooldown = Math.max(0, this.bombCooldown - delta);
    }
    this.sprite.position.set(this.position.x, this.position.y);
  }

  move(direction: Vector2, delta: number): void {
    const magnitude = Math.hypot(direction.x, direction.y);
    if (magnitude === 0) {
      return;
    }
    const normalized = { x: direction.x / magnitude, y: direction.y / magnitude };
    const speed = this.config.player.speed * (this.isSlowed ? this.config.player.slowSpeedMultiplier : 1);
    this.position.x += normalized.x * speed * delta;
    this.position.y += normalized.y * speed * delta;
    this.position.x = Math.max(24, Math.min(this.config.width - 24, this.position.x));
    this.position.y = Math.max(24, Math.min(this.config.height - 24, this.position.y));
  }

  triggerBomb(bullets: BulletPool): boolean {
    if (this.stats.bombs <= 0 || this.bombCooldown > 0) {
      return false;
    }
    this.stats.bombs -= 1;
    this.bombCooldown = 3;
    bullets.clear();
    this.invulnerableTimer = 2;
    return true;
  }

  takeHit(): boolean {
    if (this.invulnerableTimer > 0) {
      return false;
    }
    this.stats.lives -= 1;
    if (this.stats.lives < 0) {
      if (this.stats.continues > 0) {
        this.stats.continues -= 1;
        this.stats.lives = 2;
        this.stats.bombs = this.config.player.bombCount;
        this.stats.power = Math.max(1, this.stats.power * 0.5);
      } else {
        return true; // game over
      }
    }
    this.invulnerableTimer = 3;
    return false;
  }

  collectPower(value: number): void {
    this.stats.power = Math.min(8, this.stats.power + value);
  }

  collectBomb(): void {
    this.stats.bombs += 1;
  }

  collectLife(): void {
    this.stats.lives += 1;
  }

  addScore(amount: number): void {
    this.stats.score += amount;
  }

  addGraze(amount: number): void {
    this.stats.graze += amount;
  }

  serializeConfig(): PlayerConfig {
    return this.config.player;
  }
}
