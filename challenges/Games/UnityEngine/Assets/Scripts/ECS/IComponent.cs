using UnityEngine;

namespace ChallengeEngine.ECS
{
    /// <summary>
    /// Marker interface for components that can be attached to entities managed
    /// by the runtime registry.
    /// </summary>
    public interface IComponent
    {
    }

    /// <summary>
    /// Base component class that exposes the <see cref="EntityId"/> the component
    /// is attached to. MonoBehaviours can inherit this so the entity mapping
    /// stays accessible inside the Unity editor.
    /// </summary>
    public abstract class ComponentBehaviour : MonoBehaviour, IComponent
    {
        [SerializeField]
        [Tooltip("Entity that owns this component. Generated automatically when the component is registered with the world.")]
        private string entityId = string.Empty;

        public EntityId Entity { get; private set; }

        public void Bind(EntityId id)
        {
            Entity = id;
            entityId = id.ToString();
        }

        protected virtual void OnValidate()
        {
            if (!string.IsNullOrWhiteSpace(entityId) && Entity.Equals(default))
            {
                Entity = new EntityId(System.Guid.Parse(entityId));
            }
        }
    }
}
