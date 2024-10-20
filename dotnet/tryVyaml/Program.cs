using VYaml.Serialization;
using VYaml.Parser;
using System.Buffers;
using System.Text;

async Task<int> Experiment_ExploringFileContent(){
    if (args.Length < 1)
    {
	Console.WriteLine("usage: EXE UNITY-YAML-FILE");
	return -1;
    }

    string fileName = args[0];

    if (!File.Exists(fileName))
    {
	Console.WriteLine($"fatail: coul'd not find file \"{fileName}\"");
	return -1;
    }

    using var stream = File.OpenRead(args[0]);

    foreach (var component in await YamlSerializer.DeserializeMultipleDocumentsAsync<Dictionary<object,object>>(stream))
    {
	foreach (var (k, v) in component) {
	    Console.WriteLine($"`-- {k}: {v}");
	}
    }
    return 0;
}

int Experiment_AccessAnchorInformationViaParser(){
    var parser = new YamlParser(new ReadOnlySequence<byte>(Encoding.UTF8.GetBytes(new[]
    {
	"%YAML 1.1",
	"%TAG !u! tag:unity3d.com,2011:",
	"--- !u!29 &1",
	"OcclusionCullingSettings:",
	"  m_ObjectHideFlags: 0",
    }.Aggregate((a, b) => $"{a}\n{b}"))));

    parser.SkipAfter(ParseEventType.StreamStart);

    Console.WriteLine($"event: {parser.CurrentEventType}");
    parser.Read();
    Console.WriteLine($"is tag found: {parser.TryGetCurrentTag(out Tag tag)}");
    Console.WriteLine($":handle {tag.Handle} :suffix {tag.Suffix}");
    Console.WriteLine(parser.TryGetCurrentAnchor(out Anchor anchor));
    Console.WriteLine($"anchor: {anchor}");
    parser.Read();
    Console.WriteLine(parser.GetScalarAsString());

    return 0;
}

return Experiment_AccessAnchorInformationViaParser();
