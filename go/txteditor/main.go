package main
import (
	"log"
	"os"
	"github.com/muesli/termenv"
	"golang.org/x/term"
	"unicode/utf8"
)

const (
	BufTypeOrigin = iota
	BufTypeAddition
)

/// Record represents one piece in PieceTable.
/// It denote which table to use, first index in table, and length of text
type Record struct {
	bufType int
	startIdx int
	length int
}

type PieceTable struct {
	origin string
	addition string
	records []Record
}

/// Create PieceTable for given file.
func FromFile(fn string) (PieceTable, error) {
	data, err := os.ReadFile(fn)
	if err != nil {
		return PieceTable{}, err
	}

	return PieceTable {origin:string(data),
		addition: "",
		records: []Record{Record{bufType: BufTypeOrigin, startIdx: 0, length: len(data)}},
	}, nil
}

/// Returns contents of given PieceTable.
func (pt PieceTable) Contents() string {
	var ret string
	for i := 0; i < len(pt.records); i++ {
		record := pt.records[i]
		switch record.bufType {
		case BufTypeOrigin:
			ret += pt.origin[record.startIdx:record.startIdx+record.length]
		case BufTypeAddition:
			ret += pt.addition[record.startIdx:record.startIdx+record.length]
		}
	}
	return ret
}

type EditorState struct {
	exit bool
}

type Command struct {
	Exec func(st EditorState) EditorState
}

var Quit = Command { Exec: func(st EditorState) EditorState { st.exit = true; return st }}

var keymap = map[rune]Command{'q': Quit}

func main() {
	// Initiate some environment
	logger := log.Default()
	currentTerm := termenv.DefaultOutput()
	
	pt, err := FromFile("/etc/profile")
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
