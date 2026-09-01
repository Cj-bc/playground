pub mod Private {
  enum Private(Int32)
  enum NonCompanionPrivate(Int32)

  pub def make(i: Int32): Private = Private(i)
  pub def makeNonCompanion(i: Int32): NonCompanionPrivate = NonCompanionPrivate.NonCompanionPrivate(i)

  instance ToString[Private] {
    pub def toString(x: Private): String =
      let Private(n) = x;
      ToString.toString(n)
  }

  def privateMethod(): Int32 = 1
}
