pub mod HitNBlow {

  pub def eval(secret: List[PinColor], guess: List[PinColor]): {hits = Int32, blows = Int32 }
    = {
    let rules = #{
      Hit(pos, pin) :- Guess(pos, pin), Secret(pos, pin).
      Blow(pos, pin) :- Guess(pos, pin), Secret(pos2, pin), if (pos != pos2), not Hit(pos, pin).
    };

    let facts = inject List.zipWithIndex(secret), List.zipWithIndex(guess) into Secret/2, Guess/2;

    let hitsn = Vector.length(query rules, facts select pos from Hit(pos, pin));
    let blowsn = Vector.length(query rules, facts select pos from Blow(pos, pin));

    { hits = hitsn, blows = blowsn }
  }


  pub def getCandidates(_secret: List[PinColor], _history: List[{ guess = List[PinColor], hits = Int32, blows = Int32}]): List[List[PinColor]] = {
    let _rules: #{ Candidate(Int32, Int32, PinColor), Guess(Int32, Int32, PinColor),
                  SimHit(Int32, Int32, Int32), SimBlow(Int32, Int32, Int32), SimHitCount(Int32, Int32; Set[Int32])} = #{
      SimHit(candidateId, guessId, pos) :- Candidate(candidateId, pos, pin), Guess(guessId, pos, pin).
      SimHitCount(cId, gId; Set#{pos}) :- SimHit(cId, gId, pos).
    };
    List#{}
  }
}
