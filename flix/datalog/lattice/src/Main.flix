// The main entry point.
def main(): Unit \ IO =
    let rules = #{
      Eq(id; Set#{item}) :- Item(id, item).
    };
    let facts = inject List#{(0, 10), (2, 20), (3, 10)} into Item/2;

    query rules, facts select set from Eq(id; set) |> println
