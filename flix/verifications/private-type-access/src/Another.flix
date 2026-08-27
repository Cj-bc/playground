pub mod Another {
  use Sealed.{make, Sealed};

  pub def expected_to_work(): Sealed = make(1)
  pub def should_be_invalid(): Sealed = Sealed.Sealed(1)
}
