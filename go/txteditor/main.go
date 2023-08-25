package main
import (
	"log"
	"github.com/muesli/termenv"
	"golang.org/x/term"
	"unicode/utf8"
)

type Command struct {
	Exec func(st EditorState) EditorState
}

var Quit = Command { Exec: func(st EditorState) EditorState { st.exit = true; return st }}

var keymap = map[rune]Command{'q': Quit}

func main() {
	// Initiate some environment
	logger := log.Default()
	currentTerm := termenv.DefaultOutput()
	
	pt, err := PieceTableFromFile("/etc/profile")
	if err != nil {
		logger.Fatal(err)
	}

	editorState := EditorState{exit: false}

	// Enables Alt screen
	currentTerm.AltScreen()
	defer currentTerm.ExitAltScreen()
	currentTerm.Write([]byte(pt.Contents()))

	// Make TTY Raw mode so that we can read code-point per code-point
	connectedFd := int(currentTerm.TTY().Fd())
	initialEnv, err := term.MakeRaw(connectedFd)
	if err != nil {
		logger.Fatal(err)
	}
	defer term.Restore(connectedFd, initialEnv)

	// Key event loop
	var keyInput [64]byte
	for !editorState.exit {
		currentTerm.TTY().Read(keyInput[:])
		rune, _ := utf8.DecodeRune(keyInput[:])
		if command, ok := keymap[rune]; ok == true {
			editorState = command.Exec(editorState)
		}
	}
}
