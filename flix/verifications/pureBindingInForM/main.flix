

def f(): Result[Unit, Int32] =
  forM (
    _ <- Ok(1); // First line should be generator to suppress unrelated error
    const = (1)
  ) yield const

def main(): Unit \ IO = println(f())
