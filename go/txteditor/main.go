package main
import (
	"log"
	"github.com/muesli/termenv"
	"golang.org/x/term"
	"unicode/utf8"
)


func main() {
	// Initiate some environment
	logger := log.Default()
	currentTerm := termenv.DefaultOutput()
	
	buf, err := NewFileBuffer("/tmp/bashrc")
	if err != nil {
		logger.Fatal(err)
	}
	defer buf.Close()
	editorState := EditorStateWithBuffer(buf)

	// Enables Alt screen
	currentTerm.AltScreen()
	defer currentTerm.ExitAltScreen()
	
	currentTerm.Write([]byte(editorState.CurrentBuffer().Contents()))

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
		if command, ok := editorState.keymap[rune]; ok == true {
			editorState = command.Exec(editorState)
		}
	}
}
