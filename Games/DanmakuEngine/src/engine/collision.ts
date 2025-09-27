import type { BulletInstance, Hitbox, Vector2 } from './types.ts';

const distanceSq = (a: Vector2, b: Vector2): number => (a.x - b.x) ** 2 + (a.y - b.y) ** 2;

const offsetPosition = (source: Vector2, offset?: Vector2): Vector2 => ({
  x: source.x + (offset?.x ?? 0),
  y: source.y + (offset?.y ?? 0)
});

export const hitTest = (aPos: Vector2, aHitbox: Hitbox, bPos: Vector2, bHitbox: Hitbox): boolean => {
  const aPoint = offsetPosition(aPos, aHitbox.offset);
  const bPoint = offsetPosition(bPos, bHitbox.offset);
  const radius = aHitbox.radius + bHitbox.radius;
  return distanceSq(aPoint, bPoint) <= radius * radius;
};

export const grazeTest = (playerPos: Vector2, grazeRadius: number, bullet: BulletInstance): boolean => {
  const distance = Math.sqrt(distanceSq(playerPos, bullet.position));
  return distance > bullet.definition.hitbox.radius && distance <= grazeRadius;
};
