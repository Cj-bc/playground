﻿using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Grpc.Core;
using FaceDataServer;
using System.Threading.Tasks;

public class FaceData : MonoBehaviour
{
    private FaceDataServer.FaceDataServer.FaceDataServerClient client;
    // Start is called before the first frame update
    void Start()
    {
        Debug.Log("------ Top of FaceData.Start()");
        Channel channel = new Channel("127.0.0.1:50052", ChannelCredentials.Insecure);
        client = new FaceDataServer.FaceDataServer.FaceDataServerClient(channel);

        Debug.Log("----- Before InitFaceDataServer -----");
        InitFaceDataServer();
        Debug.Log("----- After InitFaceDataServer -----");

        ApplyFaceDataToModel();
        channel.ShutdownAsync().Wait();
    }

    public void InitFaceDataServer()
    {
      FaceDataServer.VoidCom vc = new FaceDataServer.VoidCom();
      Debug.Log("-- Before CLIENT.INIT call");
      FaceDataServer.Status st = client.init(vc);
      Debug.Log("-- After CLIENT.INIT call");
      if (!st.Success)
      {
        throw new Exception(st.ExitCode.ToString());
      }
    }

    public async Task ApplyFaceDataToModel()
    {
      try
      {
        FaceDataServer.VoidCom vc = new FaceDataServer.VoidCom();
        using (var call = client.startStream(vc))
        {
          var stream = call.ResponseStream;

          while (await stream.MoveNext())
          {
            FaceDataServer.FaceData fd = stream.Current;
            float angleX = fd.X * Mathf.Rad2Deg;
            float angleY = fd.Y * Mathf.Rad2Deg;
            float angleZ = fd.Z * Mathf.Rad2Deg;
            transform.Rotate(angleX, angleY, angleZ);
          }
        }
      }
      catch (RpcException e)
      {
        throw;
      }
    }
}
