#if UNITY_EDITOR
using System.IO;
using ChallengeEngine.Scenes;
using UnityEditor;
using UnityEditor.AssetImporters;

namespace ChallengeEngine.Editor
{
    /// <summary>
    /// Example ScriptedImporter that turns `.sceneasset.json` files into
    /// <see cref="SceneCatalog"/> entries. Demonstrates how future challenges can
    /// plug in custom content pipelines without leaving the editor.
    /// </summary>
    [ScriptedImporter(1, "sceneasset.json")]
    public sealed class SceneDefinitionImporter : ScriptedImporter
    {
        public override void OnImportAsset(AssetImportContext ctx)
        {
            var json = File.ReadAllText(ctx.assetPath);
            var catalog = UnityEngine.ScriptableObject.CreateInstance<SceneCatalog>();
            UnityEditor.EditorJsonUtility.FromJsonOverwrite(json, catalog);

            ctx.AddObjectToAsset("SceneCatalog", catalog);
            ctx.SetMainObject(catalog);
        }
    }
}
#endif
