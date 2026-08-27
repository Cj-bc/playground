pub mod Another {
  use Outer.{make, Outer, I};

  pub def expected_to_work(): Outer = make(1)
  pub def should_be_invalid(): Outer = Outer.Outer(I.I(1))
}
