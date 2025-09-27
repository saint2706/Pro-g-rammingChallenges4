using ChallengeEngine.ECS;
using UnityEngine;

namespace ChallengeEngine.Samples
{
    /// <summary>
    /// MonoBehaviour that owns the core <see cref="World"/> instance. Keeps the
    /// ECS runtime accessible to other behaviours so they can hook into the
    /// update loop without duplicating setup code.
    /// </summary>
    public sealed class WorldBridge : MonoBehaviour
    {
        public World World { get; } = new();

        private void Update()
        {
            World.Step(Time.deltaTime);
        }
    }
}
