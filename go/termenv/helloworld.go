// Trying termenv
package main

import (
	"github.com/muesli/termenv"
	"fmt"
)

func wrapped(s string) string {
	top, bottom := "+", "+"
	middle := "| " + s + " |"
	for i := 0; i < len(s) + 2; i++ {
		top += "-"
		bottom += "-"
	}
	top += "+"
	bottom += "+"
	return fmt.Sprint(top, "\n", middle, "\n", bottom)
}

func main() {
	o := termenv.DefaultOutput()
	defer o.Reset()

	o.ClearScreen()
	fmt.Print(wrapped("This is test program!")+"\n")
}
