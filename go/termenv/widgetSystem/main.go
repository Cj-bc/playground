// Trying termenv
package main

import (
	"github.com/muesli/termenv"
	"fmt"
	"time"
	"example.com/cjbc/termenv/widgetSystem/widgets"
	"example.com/cjbc/termenv/widgetSystem/widgets/vBox"
	"example.com/cjbc/termenv/widgetSystem/widgets/hBox"
	"example.com/cjbc/termenv/widgetSystem/widgets/border"
	"example.com/cjbc/termenv/widgetSystem/widgets/str"
	"example.com/cjbc/termenv/widgetSystem/widgets/progressBar"
)

type model struct {
	isEnded bool
	progress float64
	text string
}

func main() {
	o := termenv.DefaultOutput()
	defer o.Reset()

	model := model{}

	tick := time.Tick(5*time.Millisecond)
	timeout := time.After(5*time.Second)
	for !model.isEnded {
		select {
		case <-timeout:
			model.isEnded = true
		case <-tick:
			model.progress += 0.001
			o.ClearScreen()
			fmt.Println(view(model).Render())
		}
	}
	fmt.Println("")
}

// +-----------------------++-------------------+
// | This is test program! || Second block here |
// +-----------------------++-------------------+
// [============================================]
func view(m model) widgets.Widget {
	return vBox.New(
		hBox.New(
			border.New(str.New("This is test program!")),
			border.New(str.New("Second block here"))),
		progressBar.New(m.progress, 46),
	)
}
