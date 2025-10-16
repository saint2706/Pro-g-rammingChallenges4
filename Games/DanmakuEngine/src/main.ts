import { Application, Container, Graphics } from 'pixi.js';
import { BASE_CONFIG } from './engine/config.ts';
import { BulletPool } from './engine/bullets.ts';
import { Player } from './engine/player.ts';
import { PowerUpManager } from './engine/powerups.ts';
import { grazeTest, hitTest } from './engine/collision.ts';
import { loadBossScripts, loadStageFromFile } from './engine/scripting.ts';
import { StageRunner } from './engine/stage.ts';
import type { GameConfig, StageDefinition } from './engine/types.ts';
import { registerDefaultBullets } from './engine/entities.ts';

async function bootstrap(): Promise<void> {
  const stage = await loadStageFromFile('stages/stage1.json');
  const stageConfig = applyConfigOverrides(BASE_CONFIG, stage.configOverrides);

  const canvas = document.getElementById('game') as HTMLCanvasElement | null;
  const app = new Application();
  await app.init({
    width: stageConfig.width,
    height: stageConfig.height,
    backgroundColor: stageConfig.backgroundColor,
    view: canvas ?? undefined,
    antialias: true
  });

  const gameLayer = new Container();
  app.stage.addChild(gameLayer);

  const bullets = new BulletPool();
  bullets.setBounds({ width: stageConfig.width, height: stageConfig.height });
  registerDefaultBullets(bullets);
  gameLayer.addChild(bullets.view);

  const player = new Player(gameLayer, stageConfig);
  player.sprite.name = 'player';

  const powerUps = new PowerUpManager(gameLayer, { width: stageConfig.width, height: stageConfig.height });

  initializeStage(stage, stageConfig, app, gameLayer, bullets, player, powerUps);
}

function initializeStage(
  stage: StageDefinition,
  config: GameConfig,
  app: Application,
  layer: Container,
  bullets: BulletPool,
  player: Player,
  powerUps: PowerUpManager
): void {
  const stageRunner = new StageRunner(stage, layer, bullets, config);
  loadBossScripts('stages').then((scripts) => {
    stageRunner.attachBossScripts(scripts);
    stageRunner.startBossPhase(stage.boss);
  });

  const background = new Graphics();
  background.rect(0, 0, app.screen.width, app.screen.height).fill(0x050818);
  layer.addChildAt(background, 0);

  const input = createInputController();

  app.ticker.add((time) => {
    const delta = time.deltaMS / 1000;
    const direction = input.direction();
    player.isSlowed = input.isSlow();
    player.move(direction, delta);
    player.update(delta);

    if (input.shouldBomb()) {
      player.triggerBomb(bullets);
    }

    bullets.update(delta);
    stageRunner.update(delta);
    powerUps.update(delta, player);

    const detectionRadius =
      Math.max(config.player.hitbox.radius, config.player.grazeBox.radius) + 48;
    bullets.forEachNearby(player.position, detectionRadius, (bullet) => {
      if (
        hitTest(
          player.position,
          config.player.hitbox,
          bullet.position,
          bullet.definition.hitbox
        )
      ) {
        const gameOver = player.takeHit();
        if (gameOver) {
          app.ticker.stop();
        }
      } else if (grazeTest(player.position, config.player.grazeBox.radius, bullet)) {
        player.addGraze(bullet.definition.grazeScore ?? 1);
        player.addScore(50);
      }
    });
  });
}

function applyConfigOverrides(base: GameConfig, overrides?: Partial<GameConfig>): GameConfig {
  if (!overrides) {
    return {
      ...base,
      player: clonePlayerConfig(base.player),
      difficultyScaling: { ...base.difficultyScaling }
    };
  }
  const overridePlayer = overrides.player;
  return {
    ...base,
    ...overrides,
    player: mergePlayerConfig(base.player, overridePlayer),
    difficultyScaling: { ...base.difficultyScaling, ...overrides.difficultyScaling }
  };
}

function mergePlayerConfig(basePlayer: GameConfig['player'], override?: Partial<GameConfig['player']>) {
  const mergedHitbox = {
    ...basePlayer.hitbox,
    ...(override?.hitbox ?? {}),
    offset: {
      ...basePlayer.hitbox.offset,
      ...(override?.hitbox?.offset ?? {})
    }
  };
  const mergedGraze = {
    ...basePlayer.grazeBox,
    ...(override?.grazeBox ?? {}),
    offset: {
      ...basePlayer.grazeBox.offset,
      ...(override?.grazeBox?.offset ?? {})
    }
  };
  return {
    ...clonePlayerConfig(basePlayer),
    ...override,
    hitbox: mergedHitbox,
    grazeBox: mergedGraze
  };
}

function clonePlayerConfig(player: GameConfig['player']) {
  return {
    ...player,
    hitbox: {
      ...player.hitbox,
      offset: player.hitbox.offset ? { ...player.hitbox.offset } : undefined
    },
    grazeBox: {
      ...player.grazeBox,
      offset: player.grazeBox.offset ? { ...player.grazeBox.offset } : undefined
    }
  };
}

function createInputController() {
  const keys: Record<string, boolean> = {};

  const keyMap: Record<string, string> = {
    ArrowUp: 'up',
    ArrowDown: 'down',
    ArrowLeft: 'left',
    ArrowRight: 'right',
    ShiftLeft: 'slow',
    ShiftRight: 'slow',
    KeyZ: 'bomb'
  };

  window.addEventListener('keydown', (event) => {
    const mapped = keyMap[event.code];
    if (mapped) {
      keys[mapped] = true;
    }
  });

  window.addEventListener('keyup', (event) => {
    const mapped = keyMap[event.code];
    if (mapped) {
      keys[mapped] = false;
    }
  });

  return {
    direction: () => ({
      x: (keys.right ? 1 : 0) - (keys.left ? 1 : 0),
      y: (keys.down ? 1 : 0) - (keys.up ? 1 : 0)
    }),
    isSlow: () => Boolean(keys.slow),
    shouldBomb: () => {
      if (keys.bomb) {
        keys.bomb = false;
        return true;
      }
      return false;
    }
  };
}

document.addEventListener('DOMContentLoaded', () => {
  bootstrap().catch((error) => console.error(error));
});
