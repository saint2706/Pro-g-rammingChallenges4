using System.Collections.Generic;
using System.IO;
using System.Text;
using ChallengeEngine.ECS;
using Newtonsoft.Json;
using UnityEngine;

namespace ChallengeEngine.Serialization
{
    /// <summary>
    /// Serialises entity/component state into JSON files. Components opt-in by
    /// providing a <see cref="ISerializableComponent"/> wrapper that exposes pure
    /// data, making save data stable across play mode sessions.
    /// </summary>
    public sealed class WorldSerializer
    {
        private readonly JsonSerializerSettings _settings = new()
        {
            Formatting = Formatting.Indented,
            TypeNameHandling = TypeNameHandling.Auto
        };

        public void Save(World world, string path)
        {
            var payload = new List<ComponentSnapshot>();
            foreach (var entity in world.Entities)
            {
                if (world.TryGetComponent(entity, out SerializableEntity? serializable))
                {
                    payload.Add(serializable.ToSnapshot());
                }
            }

            var json = JsonConvert.SerializeObject(payload, _settings);
            File.WriteAllText(path, json, Encoding.UTF8);
        }

        public void Load(World world, string path)
        {
            if (!File.Exists(path))
            {
                Debug.LogWarning($"Save file '{path}' could not be found.");
                return;
            }

            var json = File.ReadAllText(path, Encoding.UTF8);
            var payload = JsonConvert.DeserializeObject<List<ComponentSnapshot>>(json, _settings);
            if (payload == null)
            {
                return;
            }

            foreach (var snapshot in payload)
            {
                var entity = world.CreateEntity();
                foreach (var componentData in snapshot.Components)
                {
                    componentData.Apply(world, entity);
                }
            }
        }
    }

    public interface ISerializableComponent
    {
        SerializableComponentData ToSerializable();
    }

    public sealed class SerializableEntity : IComponent
    {
        private readonly List<ISerializableComponent> _components;

        public SerializableEntity(List<ISerializableComponent> components)
        {
            _components = components;
        }

        public ComponentSnapshot ToSnapshot()
        {
            var snapshot = new ComponentSnapshot();
            foreach (var component in _components)
            {
                snapshot.Components.Add(component.ToSerializable());
            }

            return snapshot;
        }
    }

    public sealed class ComponentSnapshot
    {
        public List<SerializableComponentData> Components { get; } = new();
    }

    [System.Serializable]
    public abstract class SerializableComponentData
    {
        public abstract void Apply(World world, EntityId entity);
    }
}
