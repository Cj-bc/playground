import java.lang.System
import org.jline.reader.LineReaderBuilder 
import org.jline.terminal.TerminalBuilder
import org.jline.utils.{Display, AttributedString}
import org.jline.utils.{InfoCmp$Capability => Capability }

use Adaptor.toList

// The main entry point.
def main(): Unit \ IO =
    let terminal = TerminalBuilder.terminal();
    terminal.getAttributes();
    let display = new Display(terminal, true);
    let lines = toList(List#{"test", "abc", "LNFS"} |> List.map(s -> new AttributedString(s)));
    display.resize(terminal.getHeight(), terminal.getWidth());
    terminal.puts(Capability.enter_ca_mode);
    display.update(lines, 0);
    terminal.flush();
    let reader = LineReaderBuilder.builder().terminal(terminal).build();
    discard reader.readLine();
    terminal.puts(Capability.exit_ca_mode);
    terminal.flush();
    discard reader.readLine();
    terminal.close();
    ()
