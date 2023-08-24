package main

import (
	"os"
	"time"
	"golang.org/x/sys/unix"
	"unicode/utf8"
	"fmt"
)

func main() {
	f := os.Stdin

	SetNonCanonical(int(f.Fd()))
	defer SetCanonical(int(f.Fd()))

	// Setup timeout for 10 seconds
	timeoutChan := make(chan bool, 1)
	defer close(timeoutChan)
	go func() {
		time.Sleep(3 * time.Second)
		timeoutChan <- true
	}()

	// *IMPORTANT* You should specify capacity
	var buf [256]byte
OuterLoop:
	for {
		select {
		case _ = <-timeoutChan:
			break OuterLoop
		default:
			// if _, err := fmt.Fscan(f, &buf); err != nil {
			if _, err := f.Read(buf[:]); err != nil {
				fmt.Printf("Failed to read stdin: %v", err)
			} else {
				var s []rune
				w := 0
				var rune rune
				for i := 0; i < len(buf); i += w {
					rune, w = utf8.DecodeRune(buf[i:])
					s = append(s, rune)
				}
				fmt.Printf("Received rune: %#U\n", s)
			}
		}
	}
}


// Unset canonical mode to given fd
func SetNonCanonical(fd int) error {
	termios, err := unix.IoctlGetTermios(fd, unix.TCGETS)
	if err != nil {
		return err
	}

	// Unset canonical mode bit
	termios.Lflag &^= unix.ICANON
	termios.Lflag &^= unix.ECHO
	err = unix.IoctlSetTermios(fd, unix.TCSETS, termios)
	if err != nil {
		return err
	}
	return nil
}

// Set canonical mode to given fd
func SetCanonical(fd int) error {
	termios, err := unix.IoctlGetTermios(fd, unix.TCGETS)
	if err != nil {
		return err
	}

	termios.Lflag |= unix.ICANON
	termios.Lflag |= unix.ECHO
	err = unix.IoctlSetTermios(fd, unix.TCSETS, termios)
	if err != nil {
		return err
	}
	return nil
}
