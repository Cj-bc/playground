use Foldable.Foldable
use HitNBlow.{getAllCandidates, allColors}

def assertHasAllColors(v: t[PinColor]): Unit \ {Assert, Foldable.Aef[t]}  with Foldable[t] =
  use RichString.{red, text, yellow};

  let notIncludedColors = Foldable.foldLeft(
    (rest, pin) -> Set.remove(pin, rest), allColors(), v);

  if (Set.isEmpty(notIncludedColors))
    Assert.assert(true, text("expected all PinColors"))
  else 
    Assert.assert(false, red("Assertion failed: ") + text("expected All PinColors aee present, but following Colors aren't present: ") + yellow("`${notIncludedColors}`"))
  

@Test
pub def getAllCandidates_oneLength(): Unit \ Assert =
  discard getAllCandidates(1)
    |> List.groupBy(match (_, pos, _) -> pos)
    |> Functor.map(Functor.map(match (_, _, pin) -> pin)
                   >> assertHasAllColors);
  ()
  

@Test
pub def getAllCandidates_twoLength(): Unit \ Assert =
  discard getAllCandidates(2)
    |> List.groupBy(match (_, pos, _) -> pos)
    |> Functor.map(Functor.map(match (_, _, pin) -> pin)
                   >> assertHasAllColors);
  ()

@Test
pub def getAllCandidates_threeLength(): Unit \ Assert =
  discard getAllCandidates(3)
    |> List.groupBy(match (_, pos, _) -> pos)
    |> Functor.map(Functor.map(match (_, _, pin) -> pin)
                   >> assertHasAllColors);
  ()

@Test
pub def getAllCandidates_fourLength(): Unit \ Assert =
  discard getAllCandidates(4)
    |> List.groupBy(match (_, pos, _) -> pos)
    |> Functor.map(Functor.map(match (_, _, pin) -> pin)
                   >> assertHasAllColors);
  ()
