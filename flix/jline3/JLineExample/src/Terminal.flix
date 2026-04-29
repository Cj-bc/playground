import org.jline.terminal.TerminalBuilder
import java.io.{Console => JConsole }
import java.lang.System

use Sys.Console

pub eff Terminal {
  def write(str: String): Unit
  def flush(): Unit
}

pub mod Terminal {
  import org.jline.terminal.TerminalBuilder
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

  pub def runWithJline(f: Unit -> a \ af): a \ (af - Terminal + IO)
    = {
      let terminal = TerminalBuilder.builder().system(true).build();
      let ret = run { f() }
        with handler Terminal {
          def write(str, k) = {
            terminal.writer().println(str);
            k()
          }
          def flush(k) = {
            terminal.flush();
            k()
          }
        };

      terminal.close();
      ret
    }
}
