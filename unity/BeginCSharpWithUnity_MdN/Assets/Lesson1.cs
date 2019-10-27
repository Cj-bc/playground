using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Lesson1 : MonoBehaviour
{
    // Start is called before the first frame update
    void Start()
    {
      Person p = new Person();
      p.lastName = "魔王";
      p.firstName = "まぐろな";
      p.show();
      Debug.Log("私のフルネームは" + p.getFullName());
    }
}


class Person {
  public string firstName;
  public string lastName;

  public void show() {
    Debug.Log("私は" + this.firstName);
  }

  public string getFullName() {
    return lastName + firstName;
  }
}
