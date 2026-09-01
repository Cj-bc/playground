use Private.{make, Private, makeNonCompanion, NonCompanionPrivate, privateMethod}

// The main entry point.
def main(): Unit \ IO =
    // expected behavior
    let _o = make(12);
    let _ncp1 = makeNonCompanion(3);

    // expected to be accessibility violation, but works
    let _ncp = NonCompanionPrivate.NonCompanionPrivate(2);

    // why is this allowed? Private is not defined as public
    let direct = Private.Private(1);
    println(direct);

    println(privateMethod())
