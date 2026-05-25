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

    let secret = List#{PinColor.Blue, PinColor.Red, PinColor.Green, PinColor.Yellow };
    let res = eval(secret,
        List#{PinColor.Blue, PinColor.Red, PinColor.Green, PinColor.Yellow });
    println("Hit: ${res#hits}, Blow:  ${res#blows}");

    getCandidatesDemo()
    //getAllCandidatesDemo()


// def process(sec: Nel[PinColor], history: ResultHistory): Nel[PinColor] =
//     getCandidates(history);


def getCandidatesDemo(): Unit \ IO =
    use HitNBlow.getCandidates;

    let res = getCandidates(Nel.Nel(
                { guess = Nel.Nel(PinColor.Red
                                 , List#{PinColor.Blue
                                        , PinColor.Green})
                , hits = 1, blows = 2 }
              , List#{
                { guess = Nel.Nel(PinColor.Green
                                 , List#{PinColor.Blue
                                        , PinColor.Red})
                , hits = 0, blows = 3 }
              }));
    println(res)

def getAllCandidatesDemo(): Unit \ IO =
    use HitNBlow.getAllCandidates;

    getAllCandidates(2) |> println
