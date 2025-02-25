﻿using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Grpc.Core;
using FaceDataServer;
using System.Threading.Tasks;

public class FaceData : MonoBehaviour
{
    public static string currentFaceData;
    private FaceDataServer.FaceDataServer.FaceDataServerClient client;
    private FaceDataServer.Token token;

    // Start is called before the first frame update
    void Start()
    {
        currentFaceData = "X: 0.0, Y: 0.0, Z: 0.0";
        Debug.Log("------ Top of FaceData.Start()");
        Channel channel = new Channel("127.0.0.1:50052", ChannelCredentials.Insecure);
        client = new FaceDataServer.FaceDataServer.FaceDataServerClient(channel);

        Debug.Log("----- Before InitFaceDataServer -----");
        InitFaceDataServer();
        Debug.Log("----- After InitFaceDataServer -----");

        ApplyFaceDataToModel();
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

      token = st.Token;
    }

    public async Task ApplyFaceDataToModel()
    {
      try
      {
        Debug.Log("Attempting connect to server with 'startStream' call...");
        using (var call = client.startStream(token))
        {
          Debug.Log("Connected to server with 'startStream' call");
          var stream = call.ResponseStream;

          while (await stream.MoveNext())
          {
            FaceDataServer.FaceData fd = stream.Current;
            Debug.Log($"[{DateTime.Now}] Got stream.Current: {fd.ToString()}");
            float angleX = fd.X * Mathf.Rad2Deg;
            float angleY = fd.Y * Mathf.Rad2Deg;
            float angleZ = fd.Z * Mathf.Rad2Deg;
            currentFaceData = $"X: {angleX}, Y: {angleY}, Z: {angleZ}";
            transform.rotation = Quaternion.Euler(angleX, angleY, angleZ);
          }
        }
      }
      catch (RpcException e)
      {
        throw;
      }
    }

    public void onApplicationQuit()
    {
      client.stopStream(token);
      client.shutdown(new FaceDataServer.VoidCom());
    }
}
