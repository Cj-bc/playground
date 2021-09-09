import Debug "mo:base/Debug"

actor HelloActor {
  public query func hello() : async () {
    Debug.print("Hello world!\n");
  }
}
