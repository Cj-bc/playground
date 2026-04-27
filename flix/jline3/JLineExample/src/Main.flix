// The main entry point.
def main(): Unit \ IO = println("a")

def runWithTty(f: Unit -> a \ af): a \ (af - Terminal + IO)
  = run { f()} with handler Terminal {
    def write(str, k) = {
      println(str);
      k()
    }
    def flush(k) = k()
  }

def loop(): Unit \ {Ask, Terminal} = {
  let line = Ask.ask("prompt> ");
  Terminal.writeln("you entered: {line}");
  Terminal.flush();
  if (line == "exit") ()
  else loop()
}

eff Terminal {
  def write(str: String): Unit
  def flush(): Unit
}

mod Terminal {
  pub def writeln(str: String): Unit \ Terminal = Terminal.write("${str}\n")
}

eff Ask {
  def ask(prompt: String): String
}
