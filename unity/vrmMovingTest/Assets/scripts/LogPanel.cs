using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class LogPanel : MonoBehaviour
{
    // Start is called before the first frame update
    void Start()
    {
        Text xyzRotations = GetComponent<Text>();
        xyzRotations.text = "X: 0.0, Y: 0.0, Z: 0.0";
    }

    // Update is called once per frame
    void Update()
    {
        Text xyzRotations = GetComponent<Text>();
        xyzRotations.text = FaceData.currentFaceData;
    }
}
