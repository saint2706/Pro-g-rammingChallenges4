using System.Collections;
using ChallengeEngine.Events;
using UnityEngine;
using UnityEngine.SceneManagement;

namespace ChallengeEngine.Scenes
{
    /// <summary>
    /// Runtime scene loader that resolves scene tags via <see cref="SceneCatalog" />
    /// and exposes events for transitions. Intended to be dropped onto a
    /// bootstrap MonoBehaviour in sample scenes.
    /// </summary>
    public sealed class SceneLoader : MonoBehaviour
    {
        [SerializeField]
        private SceneCatalog? catalog;

        [SerializeField]
        private bool showLoadingScreen = true;

        [SerializeField]
        private CanvasGroup? loadingOverlay;

        public EventBus Events { get; } = new();

        private bool _isLoading;

        private void Awake()
        {
            if (showLoadingScreen && loadingOverlay != null)
            {
                loadingOverlay.gameObject.SetActive(false);
            }
        }

        public void LoadByTag(string tag)
        {
            if (_isLoading || catalog == null)
            {
                return;
            }

            var entry = catalog.FindByTag(tag);
            if (entry == null)
            {
                Debug.LogWarning($"Scene tag '{tag}' was not found in catalog.");
                return;
            }

            StartCoroutine(LoadAsync(entry.ScenePath));
        }

        private IEnumerator LoadAsync(string scenePath)
        {
            _isLoading = true;
            Events.Publish(new SceneLoadStarted(scenePath));

            if (showLoadingScreen && loadingOverlay != null)
            {
                loadingOverlay.gameObject.SetActive(true);
                loadingOverlay.alpha = 1f;
            }

            var operation = SceneManager.LoadSceneAsync(scenePath);
            while (!operation.isDone)
            {
                Events.Publish(new SceneLoadProgress(scenePath, operation.progress));
                yield return null;
            }

            if (showLoadingScreen && loadingOverlay != null)
            {
                loadingOverlay.alpha = 0f;
                loadingOverlay.gameObject.SetActive(false);
            }

            Events.Publish(new SceneLoadCompleted(scenePath));
            _isLoading = false;
        }
    }

    public readonly record struct SceneLoadStarted(string ScenePath);

    public readonly record struct SceneLoadProgress(string ScenePath, float Progress);

    public readonly record struct SceneLoadCompleted(string ScenePath);
}
