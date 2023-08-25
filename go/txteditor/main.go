package main
import (
	"log"
	"os"
	"github.com/muesli/termenv"
	"golang.org/x/term"
	"unicode/utf8"
)

type Record struct {
	startIdx int
	length int
}

type PieceTable struct {
	origin []string
	addition []string
	records []Record
}

type EditorState struct {
	exit bool
}
func main() {
	// Initiate some environment
	logger := log.Default()
	currentTerm := termenv.DefaultOutput()
	
	data, err := os.ReadFile("/etc/profile")
	if err != nil {
		logger.Fatal(err)
	}

	editorState := EditorState{exit: false}

	// Enables Alt screen
	currentTerm.AltScreen()
	defer currentTerm.ExitAltScreen()
	currentTerm.Write(data)

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
		if rune == 'q' {
			editorState.exit = true
		}
		}
	}
}
