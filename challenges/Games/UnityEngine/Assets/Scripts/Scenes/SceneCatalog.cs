using System.Collections.Generic;
using UnityEngine;

namespace ChallengeEngine.Scenes
{
    /// <summary>
    /// Scriptable object that lists logical scenes and the address they map to.
    /// Future challenges can extend this to plug into addressables or add
    /// platform-dependent overrides.
    /// </summary>
    [CreateAssetMenu(menuName = "Challenge Engine/Scene Catalog", fileName = "SceneCatalog")]
    public sealed class SceneCatalog : ScriptableObject
    {
        [SerializeField]
        private List<SceneEntry> scenes = new();

        private readonly Dictionary<string, SceneEntry> _lookup = new();

        public SceneEntry? FindByTag(string tag)
        {
            if (_lookup.Count == 0)
            {
                foreach (var scene in scenes)
                {
                    _lookup[scene.Tag] = scene;
                }
            }

            return _lookup.TryGetValue(tag, out var entry) ? entry : null;
        }

        public IReadOnlyList<SceneEntry> AllScenes => scenes;

        [System.Serializable]
        public sealed class SceneEntry
        {
            [SerializeField]
            private string tag = string.Empty;

            [SerializeField]
            private string scenePath = string.Empty;

            [Tooltip("Optional description displayed in the scene selection overlay")]
            [SerializeField]
            private string description = string.Empty;

            public string Tag => tag;

            public string ScenePath => scenePath;

            public string Description => description;
        }
    }
}
