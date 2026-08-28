pub mod Sealed {
  enum Sealed(Int32)
  enum NonCompanionPrivate(Int32)

  pub def make(i: Int32): Sealed = Sealed(i)
  pub def makeNonCompanion(i: Int32): NonCompanionPrivate = NonCompanionPrivate.NonCompanionPrivate(i)

  instance ToString[Sealed] {
    pub def toString(x: Sealed): String =
      let Sealed(n) = x;
      ToString.toString(n)
  }
}
