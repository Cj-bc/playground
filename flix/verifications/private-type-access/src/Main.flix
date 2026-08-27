use Sealed.{make, Sealed}

// The main entry point.
def main(): Unit \ IO =
    // expected behavior
    let _o = make(12);

    // why is this allowed? both Sealed and I are private
    let direct = Sealed.Sealed(1);
    println(direct)

