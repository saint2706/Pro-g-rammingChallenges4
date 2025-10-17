using System.Collections.Generic;
using ChallengeEngine.ECS;
using UnityEngine;

namespace ChallengeEngine.Samples
{
    /// <summary>
    /// Simple ECS system that rotates any registered <see cref="SpinComponent" />
    /// using Unity's <see cref="Transform"/> API. Demonstrates how systems can be
    /// unit-tested outside of the engine because they do not inherit from MonoBehaviour.
    /// </summary>
    public sealed class SpinSystem : MonoBehaviour, ISystem
    {
        [SerializeField]
        private WorldBridge? worldBridge;

        private World? _world;

        public int ExecutionOrder => 0;

        private void OnEnable()
        {
            _world = worldBridge != null ? worldBridge.World : null;
            _world?.RegisterSystem(this);
        }

        private void OnDisable()
        {
            _world?.UnregisterSystem(this);
        }

        public void OnRegistered(World world)
        {
            _world = world;
        }

        public void OnUnregistered(World world)
        {
        }

        public void Step(float deltaTime)
        {
            if (_world == null)
            {
                return;
            }

            foreach (var entity in _world.Entities)
            {
                if (!_world.TryGetComponent(entity, out TransformComponent? transformComponent))
                {
                    continue;
                }

                float speed = 0f;
                if (_world.TryGetComponent(entity, out SpinState? state))
                {
                    speed = state.Speed;
                }
                else if (_world.TryGetComponent(entity, out SpinComponent? component))
                {
                    speed = component.Speed;
                }
                else
                {
                    continue;
                }

                transformComponent.Transform.Rotate(Vector3.up, speed * deltaTime, Space.World);
            }
        }

        public void OnComponentAdded(EntityId entity, IComponent component)
        {
        }

        public void OnEntityDestroyed(EntityId entity, IReadOnlyDictionary<System.Type, IComponent> removedComponents)
        {
        }
    }
}
