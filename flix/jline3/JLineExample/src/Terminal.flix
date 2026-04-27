use Sys.Console

pub eff Terminal {
  def write(str: String): Unit
  def flush(): Unit
}

pub mod Terminal {
  use Sys.Console

  pub def writeln(str: String): Unit \ Terminal = Terminal.write("${str}\n")

  pub def runWithConsole(f: Unit -> a \ af): a \ (af - Terminal + Console)
    = run { f()} with handler Terminal {
      def write(str, k) = {
        Console.print(str);
        k()
      }
      def flush(k) = k()
    }
}
