using VYaml.Annotations;

// [YamlObject]
// public partial class UnityScene
// {
//     public List<UnityComponent> Components;

//     public UnityScene(List<UnityComponent> _components)
//     {
//         Components = _components;
//     }
// }

[YamlObject]
[YamlObjectUnion("!u!1", typeof(UnityComponentGameObject))]
public abstract partial class UnityComponent
{
    [YamlMember("m_ObjectHideFlags")]
    public readonly int ObjectHideFlags;
    [YamlMember("serializedVersion")]
    public readonly int SerializedVersion;

    [YamlMember("m_SceneGUID")]
    public readonly int SceneGUID;

    public UnityComponent(int objectHideFlags, int serializedVersion, int sceneGUID)
    {
        ObjectHideFlags = objectHideFlags;
        SerializedVersion = serializedVersion;
        SceneGUID = sceneGUID;
    }
}


public partial class UnityComponentGameObject : UnityComponent
{

    public UnityComponentGameObject(int objectHideFlags, int serializedVersion, int sceneGUID) : base(objectHideFlags, serializedVersion, sceneGUID)
    {
    }
}
