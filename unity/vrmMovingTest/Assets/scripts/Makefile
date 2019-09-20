PROTOC_EXEC := protoc
PROTOC_PLUGIN_EXEC := grpc_csharp_plugin

updateProto:
	 $(PROTOC_EXEC) -I. --plugin=protoc-gen-grpc=$(PROTOC_PLUGIN_EXEC) --csharp_out . --grpc_out . faceDataServer.proto


