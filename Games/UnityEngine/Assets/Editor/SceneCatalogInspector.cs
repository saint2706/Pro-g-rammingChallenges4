#if UNITY_EDITOR
using ChallengeEngine.Scenes;
using UnityEditor;
using UnityEngine;

namespace ChallengeEngine.Editor
{
    [CustomEditor(typeof(SceneCatalog))]
    public sealed class SceneCatalogInspector : UnityEditor.Editor
    {
        private SerializedProperty _scenes = null!;

        private void OnEnable()
        {
            _scenes = serializedObject.FindProperty("scenes");
        }

        public override void OnInspectorGUI()
        {
            serializedObject.Update();

            EditorGUILayout.HelpBox("Logical scene registry shared by the challenge samples. Extend this asset to map menu buttons to Unity scenes without hardcoding paths.", MessageType.Info);

            for (int i = 0; i < _scenes.arraySize; i++)
            {
                var entry = _scenes.GetArrayElementAtIndex(i);
                using (new EditorGUILayout.VerticalScope(GUI.skin.box))
                {
                    EditorGUILayout.PropertyField(entry.FindPropertyRelative("tag"), new GUIContent("Tag"));
                    EditorGUILayout.PropertyField(entry.FindPropertyRelative("scenePath"), new GUIContent("Scene Path"));
                    EditorGUILayout.PropertyField(entry.FindPropertyRelative("description"), new GUIContent("Description"));

                    using (new EditorGUILayout.HorizontalScope())
                    {
                        GUILayout.FlexibleSpace();
                        if (GUILayout.Button("Ping Scene"))
                        {
                            var path = entry.FindPropertyRelative("scenePath").stringValue;
                            var asset = AssetDatabase.LoadAssetAtPath<SceneAsset>(path);
                            if (asset != null)
                            {
                                EditorGUIUtility.PingObject(asset);
                            }
                        }

                        if (GUILayout.Button("Remove"))
                        {
                            _scenes.DeleteArrayElementAtIndex(i);
                        }
                    }
                }
            }

            EditorGUILayout.Space();
            if (GUILayout.Button("Add Scene"))
            {
                _scenes.InsertArrayElementAtIndex(_scenes.arraySize);
            }

            serializedObject.ApplyModifiedProperties();
        }
    }
}
#endif
