using ChallengeEngine.ECS;

namespace ChallengeEngine.Samples
{
    /// <summary>
    /// Pure ECS component that stores the spin speed for serialization and
    /// headless simulations. The authoring MonoBehaviour mirrors this data.
    /// </summary>
    public sealed class SpinState : IComponent
    {
        public float Speed { get; set; }

        public SpinState()
        {
        }

        public SpinState(float speed)
        {
            Speed = speed;
        }
    }
}
