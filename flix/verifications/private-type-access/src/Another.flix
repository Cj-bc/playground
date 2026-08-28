pub mod Another {
  use Private.{make, Private};

  pub def expected_to_work(): Private = make(1)
  pub def should_be_invalid(): Private = Private.Private(1)
}
