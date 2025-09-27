using System;

namespace ChallengeEngine.ECS
{
    /// <summary>
    /// Immutable identifier for an entity. Backed by a GUID so that saved data
    /// can survive scene reloads and cross-session synchronisation.
    /// </summary>
    [Serializable]
    public readonly struct EntityId : IEquatable<EntityId>
    {
        public readonly Guid Value;

        public static EntityId NewEntity() => new(Guid.NewGuid());

        public EntityId(Guid value)
        {
            Value = value;
        }

        public bool Equals(EntityId other) => Value.Equals(other.Value);

        public override bool Equals(object? obj) => obj is EntityId other && Equals(other);

        public override int GetHashCode() => Value.GetHashCode();

        public override string ToString() => Value.ToString("N");

        public static bool operator ==(EntityId left, EntityId right) => left.Equals(right);

        public static bool operator !=(EntityId left, EntityId right) => !left.Equals(right);
    }
}
