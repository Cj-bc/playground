pub mod DateTime {
  use Time.Clock
  use Time.TimeUnit

  pub enum DateTime(Int64) with Eq, Order, Hash

  pub def now(): DateTime \ Clock
   = DateTime(Clock.currentTime(TimeUnit.Seconds))

  pub def today(): DateTime \ Clock
   = DateTime(Clock.currentTime(TimeUnit.Days))
}
