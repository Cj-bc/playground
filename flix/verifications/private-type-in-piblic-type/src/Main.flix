use Outer.{make, Outer, I}

// The main entry point.
def main(): Unit \ IO =
    // expected behavior
    let _o = make(12);

    // why is this allowed? both Outer and I are private
    let direct = Outer.Outer(I.I(2));
    println(direct)

