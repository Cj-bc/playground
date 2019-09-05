﻿using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Grpc.Core;
using FaceDataServer;
using System.Threading.Tasks;

public class FaceData : MonoBehaviour
{
    private Transform neck_trans;
    // Start is called before the first frame update
    void Start()
    {
        Channel ch = new Channel("127.0.0.1:50052");
        var client = new FaceDataServer.FaceDataServerClient(channel);
        neck_trans = transform;

        InitFaceDataServer();

        ApplyFaceDataToModel();
    }

    public void InitFaceDataServer()
    {
      VoidCom vc = new VoidCom();
      Status st = client.init(vc);
      if (!st.success)
      {
        throw new Exception(st.exitCode);
      }
    }

    public async Task ApplyFaceDataToModel()
    {
      try
      {
        VoidCom vc = new VoidCom();
        using (var call = client.startStream(vc))
        {
          var stream = call.ResponseStream;

          while (await stream.MoveNext())
          {
            FaceData fd = stream.Current;
            float angleX = fd.x * Mathf.Rad2Deg;
            float angleY = fd.y * Mathf.Rad2Deg;
            float angleZ = fd.z * Mathf.Rad2Deg;
            transform.Rotate(angleX, angleY, angleZ);
          }
        }
      }
      catch (RpcException e)
      {
        throw;
      }
    }


    // Update is called once per frame
    void Update()
    {
        
    }
}
