using ChallengeEngine.ECS;
using ChallengeEngine.Serialization;
using UnityEngine;

namespace ChallengeEngine.Samples
{
    /// <summary>
    /// Demo component that rotates the owning GameObject to showcase system
    /// updates. The <see cref="SpinSystem"/> handles the actual rotation logic so
    /// the behaviour can run in edit mode or play mode.
    /// </summary>
    public sealed class SpinComponent : ComponentBehaviour, ISerializableComponent
    {
        [SerializeField]
        private float speed = 45f;

        public float Speed => speed;

        public SerializableComponentData ToSerializable() => new SpinComponentData
        {
            Speed = speed
        };

        [System.Serializable]
        private sealed class SpinComponentData : SerializableComponentData
        {
            public float Speed;

            public override void Apply(World world, EntityId entity)
            {
                world.AddComponent(entity, new SpinState(Speed));
            }
        }
    }
}
