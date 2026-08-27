use Sealed.{make, Sealed}

// The main entry point.
def main(): Unit \ IO =
    // expected behavior
    let _o = make(12);

    // why is this allowed? Sealed is not defined as public
    let direct = Sealed.Sealed(1);
    println(direct)

