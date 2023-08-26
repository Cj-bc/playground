package main
import (
	"log"
	"github.com/muesli/termenv"
	"golang.org/x/sys/unix"
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
	
	// Make TTY Raw mode so that we can read code-point per code-point
	connectedFd := int(currentTerm.TTY().Fd())
	err = setupTerminal(connectedFd)
	if err != nil {
		logger.Fatal(err)
	}
	defer shutdownTerminal(connectedFd)

	// Paint once before waiting key input
	currentTerm.MoveCursor(0, 0)
	currentTerm.Write([]byte(editorState.CurrentBuffer().Contents()))

	// Key event loop
	var keyInput [64]byte
	for !editorState.exit {
		currentTerm.TTY().Read(keyInput[:])
		rune, _ := utf8.DecodeRune(keyInput[:])
		if command, ok := editorState.keymap[rune]; ok == true {
			editorState = command.Exec(editorState)
		}

		currentTerm.MoveCursor(0, 0)
		currentTerm.Write([]byte(editorState.CurrentBuffer().Contents()))
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
