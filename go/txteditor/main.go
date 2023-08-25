package main
import (
	"log"
	"os"
	"github.com/muesli/termenv"
	"golang.org/x/term"
	"fmt"
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

func main() {
	logger := log.Default()
	currentTerm := termenv.DefaultOutput()
	
	data, err := os.ReadFile("/etc/profile")
	if err != nil {
		logger.Fatal(err)
	}

	currentTerm.AltScreen()
	defer currentTerm.ExitAltScreen()
	currentTerm.Write(data)

	connectedFd := int(currentTerm.TTY().Fd())
	initialEnv, err := term.MakeRaw(connectedFd)
	if err != nil {
		logger.Fatal(err)
	}
	defer term.Restore(connectedFd, initialEnv)

	var tmp [64]byte
	for i := 0; i < 10; i++ {
		
		currentTerm.TTY().Read(tmp[:])
		rune, _ := utf8.DecodeRune(tmp[:])
		if rune == 'q' {
			break
		}
	}
}
