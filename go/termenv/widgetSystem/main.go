// Trying termenv
package main

import (
	"github.com/muesli/termenv"
	"fmt"
	"example.com/cjbc/termenv/widgetSystem/widgets/vBox"
	"example.com/cjbc/termenv/widgetSystem/widgets/hBox"
	"example.com/cjbc/termenv/widgetSystem/widgets/border"
	"example.com/cjbc/termenv/widgetSystem/widgets/str"
	"example.com/cjbc/termenv/widgetSystem/widgets/progressBar"
)

type State struct {
	isEnded bool
	progress float64
	text string
}

func main() {
	o := termenv.DefaultOutput()
	defer o.Reset()

	o.ClearScreen()
	//o.WriteString(wrapped("Hello World!")+"\n")

	// +-----------------------++-------------------+
	// | This is test program! || Second block here |
	// +-----------------------++-------------------+
	// [============================================]
	//container()
	ui := vBox.New(
		hBox.New(
			border.New(str.New("This is test program!")),
			border.New(str.New("Second block here"))),
		str.New("This should be below those two components, and be expanded"),
		progressBar.New(0.25, 30),
	)

	fmt.Println(ui.Render())

// 	var state State
// 	for !state.isEnded {
// 		if 
// 	}
}

// ------------ Rendering

// func progressBar() Widget {
// 	return func () string {return "[===]"}
// }
