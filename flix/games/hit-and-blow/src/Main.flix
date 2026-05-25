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
    use HitNBlow.eval;

    let res = eval(List#{PinColor.Blue, PinColor.Red, PinColor.Green, PinColor.Yellow },
        List#{PinColor.Blue, PinColor.Red, PinColor.Green, PinColor.Yellow });
    println("Hit: ${res#hits}, Blow:  ${res#blows}");

    getCandidatesDemo()
    //getAllCandidatesDemo()


def getCandidatesDemo(): Unit \ IO =
    use HitNBlow.getCandidates;

    let res = getCandidates(Nel.Nel({ guess = Nel.Nel(PinColor.Red, List#{PinColor.Blue}), hits = 1, blows = 1 }, List#{}));
    println(res)

def getAllCandidatesDemo(): Unit \ IO =
    use HitNBlow.getAllCandidates;

    getAllCandidates(2) |> println
