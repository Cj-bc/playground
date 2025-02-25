// <auto-generated>
//     Generated by the protocol buffer compiler.  DO NOT EDIT!
//     source: faceDataServer.proto
// </auto-generated>
#pragma warning disable 0414, 1591
#region Designer generated code

using grpc = global::Grpc.Core;

namespace FaceDataServer {
  public static partial class FaceDataServer
  {
    static readonly string __ServiceName = "FaceDataServer.FaceDataServer";

    static readonly grpc::Marshaller<global::FaceDataServer.VoidCom> __Marshaller_FaceDataServer_VoidCom = grpc::Marshallers.Create((arg) => global::Google.Protobuf.MessageExtensions.ToByteArray(arg), global::FaceDataServer.VoidCom.Parser.ParseFrom);
    static readonly grpc::Marshaller<global::FaceDataServer.Status> __Marshaller_FaceDataServer_Status = grpc::Marshallers.Create((arg) => global::Google.Protobuf.MessageExtensions.ToByteArray(arg), global::FaceDataServer.Status.Parser.ParseFrom);
    static readonly grpc::Marshaller<global::FaceDataServer.Token> __Marshaller_FaceDataServer_Token = grpc::Marshallers.Create((arg) => global::Google.Protobuf.MessageExtensions.ToByteArray(arg), global::FaceDataServer.Token.Parser.ParseFrom);
    static readonly grpc::Marshaller<global::FaceDataServer.FaceData> __Marshaller_FaceDataServer_FaceData = grpc::Marshallers.Create((arg) => global::Google.Protobuf.MessageExtensions.ToByteArray(arg), global::FaceDataServer.FaceData.Parser.ParseFrom);

    static readonly grpc::Method<global::FaceDataServer.VoidCom, global::FaceDataServer.Status> __Method_init = new grpc::Method<global::FaceDataServer.VoidCom, global::FaceDataServer.Status>(
        grpc::MethodType.Unary,
        __ServiceName,
        "init",
        __Marshaller_FaceDataServer_VoidCom,
        __Marshaller_FaceDataServer_Status);

    static readonly grpc::Method<global::FaceDataServer.Token, global::FaceDataServer.FaceData> __Method_startStream = new grpc::Method<global::FaceDataServer.Token, global::FaceDataServer.FaceData>(
        grpc::MethodType.ServerStreaming,
        __ServiceName,
        "startStream",
        __Marshaller_FaceDataServer_Token,
        __Marshaller_FaceDataServer_FaceData);

    static readonly grpc::Method<global::FaceDataServer.Token, global::FaceDataServer.Status> __Method_stopStream = new grpc::Method<global::FaceDataServer.Token, global::FaceDataServer.Status>(
        grpc::MethodType.Unary,
        __ServiceName,
        "stopStream",
        __Marshaller_FaceDataServer_Token,
        __Marshaller_FaceDataServer_Status);

    static readonly grpc::Method<global::FaceDataServer.VoidCom, global::FaceDataServer.Status> __Method_shutdown = new grpc::Method<global::FaceDataServer.VoidCom, global::FaceDataServer.Status>(
        grpc::MethodType.Unary,
        __ServiceName,
        "shutdown",
        __Marshaller_FaceDataServer_VoidCom,
        __Marshaller_FaceDataServer_Status);

    /// <summary>Service descriptor</summary>
    public static global::Google.Protobuf.Reflection.ServiceDescriptor Descriptor
    {
      get { return global::FaceDataServer.FaceDataServerReflection.Descriptor.Services[0]; }
    }

    /// <summary>Base class for server-side implementations of FaceDataServer</summary>
    [grpc::BindServiceMethod(typeof(FaceDataServer), "BindService")]
    public abstract partial class FaceDataServerBase
    {
      public virtual global::System.Threading.Tasks.Task<global::FaceDataServer.Status> init(global::FaceDataServer.VoidCom request, grpc::ServerCallContext context)
      {
        throw new grpc::RpcException(new grpc::Status(grpc::StatusCode.Unimplemented, ""));
      }

      public virtual global::System.Threading.Tasks.Task startStream(global::FaceDataServer.Token request, grpc::IServerStreamWriter<global::FaceDataServer.FaceData> responseStream, grpc::ServerCallContext context)
      {
        throw new grpc::RpcException(new grpc::Status(grpc::StatusCode.Unimplemented, ""));
      }

      public virtual global::System.Threading.Tasks.Task<global::FaceDataServer.Status> stopStream(global::FaceDataServer.Token request, grpc::ServerCallContext context)
      {
        throw new grpc::RpcException(new grpc::Status(grpc::StatusCode.Unimplemented, ""));
      }

      public virtual global::System.Threading.Tasks.Task<global::FaceDataServer.Status> shutdown(global::FaceDataServer.VoidCom request, grpc::ServerCallContext context)
      {
        throw new grpc::RpcException(new grpc::Status(grpc::StatusCode.Unimplemented, ""));
      }

    }

    /// <summary>Client for FaceDataServer</summary>
    public partial class FaceDataServerClient : grpc::ClientBase<FaceDataServerClient>
    {
      /// <summary>Creates a new client for FaceDataServer</summary>
      /// <param name="channel">The channel to use to make remote calls.</param>
      public FaceDataServerClient(grpc::ChannelBase channel) : base(channel)
      {
      }
      /// <summary>Creates a new client for FaceDataServer that uses a custom <c>CallInvoker</c>.</summary>
      /// <param name="callInvoker">The callInvoker to use to make remote calls.</param>
      public FaceDataServerClient(grpc::CallInvoker callInvoker) : base(callInvoker)
      {
      }
      /// <summary>Protected parameterless constructor to allow creation of test doubles.</summary>
      protected FaceDataServerClient() : base()
      {
      }
      /// <summary>Protected constructor to allow creation of configured clients.</summary>
      /// <param name="configuration">The client configuration.</param>
      protected FaceDataServerClient(ClientBaseConfiguration configuration) : base(configuration)
      {
      }

      public virtual global::FaceDataServer.Status init(global::FaceDataServer.VoidCom request, grpc::Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
      {
        return init(request, new grpc::CallOptions(headers, deadline, cancellationToken));
      }
      public virtual global::FaceDataServer.Status init(global::FaceDataServer.VoidCom request, grpc::CallOptions options)
      {
        return CallInvoker.BlockingUnaryCall(__Method_init, null, options, request);
      }
      public virtual grpc::AsyncUnaryCall<global::FaceDataServer.Status> initAsync(global::FaceDataServer.VoidCom request, grpc::Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
      {
        return initAsync(request, new grpc::CallOptions(headers, deadline, cancellationToken));
      }
      public virtual grpc::AsyncUnaryCall<global::FaceDataServer.Status> initAsync(global::FaceDataServer.VoidCom request, grpc::CallOptions options)
      {
        return CallInvoker.AsyncUnaryCall(__Method_init, null, options, request);
      }
      public virtual grpc::AsyncServerStreamingCall<global::FaceDataServer.FaceData> startStream(global::FaceDataServer.Token request, grpc::Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
      {
        return startStream(request, new grpc::CallOptions(headers, deadline, cancellationToken));
      }
      public virtual grpc::AsyncServerStreamingCall<global::FaceDataServer.FaceData> startStream(global::FaceDataServer.Token request, grpc::CallOptions options)
      {
        return CallInvoker.AsyncServerStreamingCall(__Method_startStream, null, options, request);
      }
      public virtual global::FaceDataServer.Status stopStream(global::FaceDataServer.Token request, grpc::Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
      {
        return stopStream(request, new grpc::CallOptions(headers, deadline, cancellationToken));
      }
      public virtual global::FaceDataServer.Status stopStream(global::FaceDataServer.Token request, grpc::CallOptions options)
      {
        return CallInvoker.BlockingUnaryCall(__Method_stopStream, null, options, request);
      }
      public virtual grpc::AsyncUnaryCall<global::FaceDataServer.Status> stopStreamAsync(global::FaceDataServer.Token request, grpc::Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
      {
        return stopStreamAsync(request, new grpc::CallOptions(headers, deadline, cancellationToken));
      }
      public virtual grpc::AsyncUnaryCall<global::FaceDataServer.Status> stopStreamAsync(global::FaceDataServer.Token request, grpc::CallOptions options)
      {
        return CallInvoker.AsyncUnaryCall(__Method_stopStream, null, options, request);
      }
      public virtual global::FaceDataServer.Status shutdown(global::FaceDataServer.VoidCom request, grpc::Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
      {
        return shutdown(request, new grpc::CallOptions(headers, deadline, cancellationToken));
      }
      public virtual global::FaceDataServer.Status shutdown(global::FaceDataServer.VoidCom request, grpc::CallOptions options)
      {
        return CallInvoker.BlockingUnaryCall(__Method_shutdown, null, options, request);
      }
      public virtual grpc::AsyncUnaryCall<global::FaceDataServer.Status> shutdownAsync(global::FaceDataServer.VoidCom request, grpc::Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
      {
        return shutdownAsync(request, new grpc::CallOptions(headers, deadline, cancellationToken));
      }
      public virtual grpc::AsyncUnaryCall<global::FaceDataServer.Status> shutdownAsync(global::FaceDataServer.VoidCom request, grpc::CallOptions options)
      {
        return CallInvoker.AsyncUnaryCall(__Method_shutdown, null, options, request);
      }
      /// <summary>Creates a new instance of client from given <c>ClientBaseConfiguration</c>.</summary>
      protected override FaceDataServerClient NewInstance(ClientBaseConfiguration configuration)
      {
        return new FaceDataServerClient(configuration);
      }
    }

    /// <summary>Creates service definition that can be registered with a server</summary>
    /// <param name="serviceImpl">An object implementing the server-side handling logic.</param>
    public static grpc::ServerServiceDefinition BindService(FaceDataServerBase serviceImpl)
    {
      return grpc::ServerServiceDefinition.CreateBuilder()
          .AddMethod(__Method_init, serviceImpl.init)
          .AddMethod(__Method_startStream, serviceImpl.startStream)
          .AddMethod(__Method_stopStream, serviceImpl.stopStream)
          .AddMethod(__Method_shutdown, serviceImpl.shutdown).Build();
    }

    /// <summary>Register service method with a service binder with or without implementation. Useful when customizing the  service binding logic.
    /// Note: this method is part of an experimental API that can change or be removed without any prior notice.</summary>
    /// <param name="serviceBinder">Service methods will be bound by calling <c>AddMethod</c> on this object.</param>
    /// <param name="serviceImpl">An object implementing the server-side handling logic.</param>
    public static void BindService(grpc::ServiceBinderBase serviceBinder, FaceDataServerBase serviceImpl)
    {
      serviceBinder.AddMethod(__Method_init, serviceImpl == null ? null : new grpc::UnaryServerMethod<global::FaceDataServer.VoidCom, global::FaceDataServer.Status>(serviceImpl.init));
      serviceBinder.AddMethod(__Method_startStream, serviceImpl == null ? null : new grpc::ServerStreamingServerMethod<global::FaceDataServer.Token, global::FaceDataServer.FaceData>(serviceImpl.startStream));
      serviceBinder.AddMethod(__Method_stopStream, serviceImpl == null ? null : new grpc::UnaryServerMethod<global::FaceDataServer.Token, global::FaceDataServer.Status>(serviceImpl.stopStream));
      serviceBinder.AddMethod(__Method_shutdown, serviceImpl == null ? null : new grpc::UnaryServerMethod<global::FaceDataServer.VoidCom, global::FaceDataServer.Status>(serviceImpl.shutdown));
    }

  }
}
#endregion
