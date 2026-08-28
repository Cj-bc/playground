use Sealed.{make, Sealed, NonCompanionPrivate}

// The main entry point.
def main(): Unit \ IO =
    // expected behavior
    let _o = make(12);
    let _ncp = NonCompanionPrivate.NonCompanionPrivate(2);

    // why is this allowed? Sealed is not defined as public
    let direct = Sealed.Sealed(1);
    println(direct)

