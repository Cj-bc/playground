using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ChickGenerator : MonoBehaviour
{
    public GameObject obj;
    public float interval = 3.0f;
    // Start is called before the first frame update
    void Start()
    {
        InvokeRepeating("SpawnObj", 0.1f, interval);
    }

    void SpawnObj()
    {
        Instantiate(obj, transform.position, transform.rotation);
    }
}
