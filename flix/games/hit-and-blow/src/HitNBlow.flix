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


  pub def getCandidates(history: Nel[{ guess = Nel[PinColor], hits = Int32, blows = Int32}]): List[Nel[(Int32, PinColor)]] = {
    let rules: #{ Candidate(Int32, Int32, PinColor)
                , Guess(Int32, Int32, PinColor)
                , SimHit(Int32, Int32, Int32)
                , SimBlow(Int32, Int32, Int32)
                , SimHitCount(Int32, Int32; Set[Int32])
                , HistoryResult(Int32, Int32, Int32)
                , ValidFor(Int32, Int32)
                , Invalid(Int32)
                , Valid(Int32)
                }
      = #{
      SimHit(candidateId, guessId, pos) :- Candidate(candidateId, pos, pin), Guess(guessId, pos, pin).
      SimHitCount(cId, gId; Set#{pos}) :- SimHit(cId, gId, pos).
      ValidFor(cId, gId) :- HistoryResult(gId, hits, blows), SimHitCount(cId, gId; sets), if (Set.count(_ -> true, sets) == hits).
      Invalid(cId) :- Candidate(cId, _, _), Guess(gId, _, _), not ValidFor(cId, gId).
      Valid(cId) :- Candidate(cId, _, _), not Invalid(cId).
    };

    let historyGuesses = inject (Nel.mapWithIndex((idx, h) -> ((idx, h#guess): (Int32, Nel[PinColor])), history) |> Nel.flatMap(match (idx, guessPins) -> Nel.mapWithIndex((pos, pin) -> ((idx, pos, pin) : (Int32, Int32, PinColor)), guessPins))) into Guess/3;
    let historyResults = inject Nel.mapWithIndex((idx, h) -> (idx, h#hits, h#blows), history) into HistoryResult/3;

    query rules,historyGuesses,historyResults,getAllCandidates(2)
    select (cId, pos, pin) from Valid(cId),Candidate(cId, pos, pin)
      |> Vector.toList
      |> List.groupBy(match (cId, _, _) -> cId) 
      |> Functor.map(Functor.map(match (_, pos, pin) -> (pos, pin)))
  }

   pub def getAllCandidates(len: Int32): #{ Candidate(Int32, Int32, PinColor) | r } =
    let allColors = List#{ PinColor.Blue, PinColor.Red
                    , PinColor.Green, PinColor.Yellow
                    , PinColor.Pink, PinColor.White };

    def mkCombo(level: Int32, cands: List[List[PinColor]]): List[List[PinColor]] =
      if (level == 0) cands
      else ((forM (cand <- cands; pin <- allColors)
        yield pin :: cand)
        |> mkCombo (level - 1));

    let allCands = mkCombo(len - 1, allColors |> List.map(pin -> pin :: Nil))
      |> List.mapWithIndex((id, l) ->
          List.mapWithIndex((pos, pin) -> (id, pos, pin), l))
      |> List.flatten;

    inject allCands into Candidate/3
}
