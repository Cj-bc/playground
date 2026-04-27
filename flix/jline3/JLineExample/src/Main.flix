use Sys.Console

// The main entry point.
def main(): Unit \ IO = run {
    loop()
  }
  with Terminal.runWithConsole
  with Ask.runWithConsole
  with Console.runWithIO

def loop(): Unit \ {Ask, Terminal} = {
  let line = Ask.ask("prompt> ");
  Terminal.writeln("you entered: {line}");
  Terminal.flush();
  if (line == "exit") ()
  else loop()
}
