using System.Collections.Generic;
using ChallengeEngine.ECS;
using ChallengeEngine.Serialization;
using UnityEngine;

namespace ChallengeEngine.Samples
{
    /// <summary>
    /// Converts attached <see cref="ComponentBehaviour"/> instances into ECS
    /// entities at runtime. This keeps the authoring workflow Unity-friendly while
    /// relying on the core world for simulation.
    /// </summary>
    [DisallowMultipleComponent]
    public sealed class EntityAuthoring : MonoBehaviour
    {
        [SerializeField]
        private WorldBridge? worldBridge;

        private EntityId _entity;
        private readonly List<ComponentBehaviour> _components = new();
        private readonly List<ISerializableComponent> _saveable = new();

        private void Awake()
        {
            if (worldBridge == null)
            {
                Debug.LogError("WorldBridge is not assigned on EntityAuthoring.");
                return;
            }

            _components.Clear();
            _saveable.Clear();
            GetComponents(_components);
            _entity = worldBridge.World.CreateEntity();
            foreach (var component in _components)
            {
                component.Bind(_entity);
                worldBridge.World.AddComponent(_entity, component);

                if (component is ISerializableComponent serializable)
                {
                    _saveable.Add(serializable);
                }

                if (component is SpinComponent spinComponent)
                {
                    worldBridge.World.AddComponent(_entity, new SpinState(spinComponent.Speed));
                }
            }

            if (_saveable.Count > 0)
            {
                worldBridge.World.AddComponent(_entity, new SerializableEntity(new List<ISerializableComponent>(_saveable)));
            }
        }

        private void OnDestroy()
        {
            if (worldBridge != null)
            {
                worldBridge.World.DestroyEntity(_entity);
            }

            _saveable.Clear();
        }
    }
}
