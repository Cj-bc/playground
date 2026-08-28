use Private.{make, Private, NonCompanionPrivate}

// The main entry point.
def main(): Unit \ IO =
    // expected behavior
    let _o = make(12);
    let _ncp = NonCompanionPrivate.NonCompanionPrivate(2);

    // why is this allowed? Private is not defined as public
    let direct = Private.Private(1);
    println(direct)

