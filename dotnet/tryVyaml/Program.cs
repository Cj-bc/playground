using VYaml.Serialization;

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

return await Experiment_ExploringFileContent();
