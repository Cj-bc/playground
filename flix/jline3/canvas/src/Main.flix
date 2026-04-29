import java.lang.System
import org.jline.reader.LineReaderBuilder 
import org.jline.terminal.TerminalBuilder
import org.jline.utils.Display

use Adaptor.toList

// The main entry point.
def main(): Unit \ IO =
    let terminal = TerminalBuilder.terminal();
    terminal.getAttributes();
    let display = new Display(terminal, true);
    let lines = toList(List#{"test", "abc", "LNFS"});
    display.resize(terminal.getHeight(), terminal.getWidth());
    display.updateAnsi(lines, 0);
    terminal.flush();
    let reader = LineReaderBuilder.builder().terminal(terminal).build();
    discard reader.readLine();
    ()
