pub enum PinColor with ToString, Eq, Order {
  case Blue,
  case Red,
  case Green,
  case Yellow,
  case Pink,
  case White
}

// The main entry point.
def main(): Unit \ IO =

    // hitand blow
    let rules = #{
      Hit(pos, pin) :- Guess(pos, pin), Secret(pos, pin).
      Blow(pos, pin) :- Guess(pos, pin), Secret(pos2, pin), if (pos != pos2), not Hit(pos, pin).
    };

    let facts = inject 
        List#{(0, PinColor.Blue), (1, PinColor.Red), (2, PinColor.Green), (3, PinColor.Yellow) },
        List#{(1, PinColor.Blue), (0, PinColor.Red), (2, PinColor.Green), (3, PinColor.Yellow) }
      into Secret/2, Guess/2;

    query rules, facts select pos from Hit(pos, pin) |> println;
    query rules, facts select pos from Blow(pos, pin) |> println
