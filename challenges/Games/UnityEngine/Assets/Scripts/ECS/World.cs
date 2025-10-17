using System;
using System.Collections.Generic;

namespace ChallengeEngine.ECS
{
    /// <summary>
    /// Lightweight entity registry that keeps track of component membership and
    /// executes registered systems in order. Systems opt into update loops so
    /// gameplay code can stay deterministic in edit mode and at runtime.
    /// </summary>
    public sealed class World
    {
        private readonly Dictionary<EntityId, Dictionary<Type, IComponent>> _componentLookup = new();
        private readonly List<ISystem> _systems = new();

        public IReadOnlyCollection<EntityId> Entities => _componentLookup.Keys;

        public EntityId CreateEntity(params IComponent[] components)
        {
            var id = EntityId.NewEntity();
            foreach (var component in components)
            {
                AddComponent(id, component);
            }

            if (!_componentLookup.ContainsKey(id))
            {
                _componentLookup[id] = new Dictionary<Type, IComponent>();
            }

            return id;
        }

        public void DestroyEntity(EntityId entity)
        {
            if (_componentLookup.Remove(entity, out var components))
            {
                foreach (var system in _systems)
                {
                    system.OnEntityDestroyed(entity, components);
                }
            }
        }

        public void AddComponent(EntityId entity, IComponent component)
        {
            if (!_componentLookup.TryGetValue(entity, out var components))
            {
                components = new Dictionary<Type, IComponent>();
                _componentLookup[entity] = components;
            }

            components[component.GetType()] = component;

            foreach (var system in _systems)
            {
                system.OnComponentAdded(entity, component);
            }
        }

        public bool TryGetComponent<T>(EntityId entity, out T? component) where T : class, IComponent
        {
            component = null;
            if (_componentLookup.TryGetValue(entity, out var components) &&
                components.TryGetValue(typeof(T), out var stored))
            {
                component = (T)stored;
                return true;
            }

            return false;
        }

        public void RegisterSystem(ISystem system)
        {
            if (_systems.Contains(system))
            {
                return;
            }

            _systems.Add(system);
            _systems.Sort((a, b) => a.ExecutionOrder.CompareTo(b.ExecutionOrder));
            system.OnRegistered(this);
        }

        public void UnregisterSystem(ISystem system)
        {
            if (_systems.Remove(system))
            {
                system.OnUnregistered(this);
            }
        }

        public void Step(float deltaTime)
        {
            foreach (var system in _systems)
            {
                system.Step(deltaTime);
            }
        }
    }

    public interface ISystem
    {
        int ExecutionOrder { get; }

        void OnRegistered(World world);

        void OnUnregistered(World world);

        void Step(float deltaTime);

        void OnComponentAdded(EntityId entity, IComponent component);

        void OnEntityDestroyed(EntityId entity, IReadOnlyDictionary<Type, IComponent> removedComponents);
    }
}
