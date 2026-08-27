pub mod Outer {
  enum Outer(I)
  enum I(Int32)

  pub def make(i: Int32): Outer = Outer(I.I(i))

  instance ToString[Outer] {
    pub def toString(x: Outer): String =
      let Outer(I.I(n)) = x;
      ToString.toString(n)
  }
}
