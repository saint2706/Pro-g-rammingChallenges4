using ChallengeEngine.ECS;
using UnityEngine;

namespace ChallengeEngine.Samples
{
    /// <summary>
    /// Wraps a <see cref="Transform"/> so the ECS world can manipulate scene
    /// objects without each system holding direct GameObject references.
    /// </summary>
    [RequireComponent(typeof(Transform))]
    public sealed class TransformComponent : ComponentBehaviour
    {
        public Transform Transform => transform;
    }
}
