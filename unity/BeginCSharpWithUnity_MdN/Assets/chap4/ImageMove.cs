using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ImageMove : MonoBehaviour
{
    Vector3 startPos;
    // Start is called before the first frame update
    void Start()
    {
        Vector3 pos = this.GetComponent<Transform>().position;
        this.startPos = pos;
    }

    // Update is called once per frame
    void Update()
    {
        Transform tr = this.GetComponent<Transform> ();
        Vector3 pos = tr.position;
        pos.x = Random.Range(-2.0f, 2.0f);
        pos.y = Random.Range(-2.0f, 2.0f);
        tr.position = pos - this.startPos;
    }
}
