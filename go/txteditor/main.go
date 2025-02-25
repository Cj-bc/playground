package main
import (
	"log"
	"github.com/muesli/termenv"
	"golang.org/x/sys/unix"
	"unicode/utf8"
	"flag"
)


func main() {
	// Initiate some environment
	logger := log.Default()
	currentTerm := termenv.DefaultOutput()

	flag.Parse()
	fn := flag.Arg(0)
	if fn == "" {
		logger.Fatal("Please specify path")
		return
	}
	
	buf, err := NewFileBuffer(fn)
	if err != nil {
		logger.Fatal(err)
	}
	defer buf.Close()
	editorState := EditorStateWithBuffer(buf)

	// Enables Alt screen
	currentTerm.AltScreen()
	defer currentTerm.ExitAltScreen()
	
	// Make TTY Raw mode so that we can read code-point per code-point
	connectedFd := int(currentTerm.TTY().Fd())
	err = setupTerminal(connectedFd)
	if err != nil {
		logger.Fatal(err)
	}
	defer shutdownTerminal(connectedFd)

	// Paint once before waiting key input
	Draw(currentTerm, editorState)

	// Key event loop
	var keyInput [64]byte
	for !editorState.exit {
		currentTerm.TTY().Read(keyInput[:])
		rune, _ := utf8.DecodeRune(keyInput[:])
		if command, ok := editorState.keymap[rune]; ok == true {
			editorState = command.Exec(editorState)
		}

		Draw(currentTerm, editorState)
	}
}

func Draw(term *termenv.Output, state EditorState) {
	term.MoveCursor(0, 0)
	term.Write([]byte(state.CurrentBuffer().Contents()))
	x, y, err := state.CurrentBuffer().PointCoord()
	if err != nil {
		state.errors = append(state.errors, fmt.Errorf("Error occured; reset cursor to 0: %w", err))
		state.CurrentBuffer().point = 0
	}
	defer term.MoveCursor(y, x)

	_, h, _ := getWinSize(int(term.TTY().Fd()))

	// Display errors at bottom
	term.MoveCursor(h-len(state.errors), 0)
	for i := 0; i < len(state.errors); i++ {
		if state.errors[i] == nil {
			continue
		}
		term.Write([]byte(fmt.Sprint(state.errors[i])))
		term.MoveCursor(h-len(state.errors)+i, 0)
		state.errors[i] = nil
	}
	clear(state.errors)
	state.errors = state.errors[:0]

	term.MoveCursor(y, x)
}

func setupTerminal(fd int) error {
	termios, err := unix.IoctlGetTermios(fd, unix.TCGETS)
	if err != nil {
		return err
	}

	termios.Lflag &^= (unix.ICANON | unix.ECHO)
	err = unix.IoctlSetTermios(fd, unix.TCSETS, termios)
	if err != nil {
		return err
	}

	return nil
}

func shutdownTerminal(fd int) error {
	termios, err := unix.IoctlGetTermios(fd, unix.TCGETS)
	if err != nil {
		return err
	}

	termios.Lflag |= (unix.ICANON | unix.ECHO)
	err = unix.IoctlSetTermios(fd, unix.TCSETS, termios)
	if err != nil {
		return err
	}

	return nil
}

func getWinSize(fd int) (int, int, error) {
	ws := unix.Winsize{}
	err := unix.IoctlSetWinsize(fd, unix.TIOCGWINSZ, &ws)
	if err != nil {
		return 0, 0, err
	}

	return int(ws.Col), int(ws.Row), nil
}
