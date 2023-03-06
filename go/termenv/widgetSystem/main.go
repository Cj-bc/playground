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

type sampleModel struct {
	progress float64
	text string
}

type model interface {
	view() widgets.Widget
	update() model
}

type state struct {
	isEnded bool
	model model
}

func main() {
	o := termenv.DefaultOutput()
	defer o.Reset()
	state := state{model: sampleModel{}}

	tick := time.Tick(5*time.Millisecond)
	timeout := time.After(5*time.Second)
	for !state.isEnded {
		select {
		case <-timeout:
			state.isEnded = true
		case <-tick:
			m := state.model.update()
			state.model = m
			o.ClearScreen()
			fmt.Println(state.model.view().Render())
		}
	}
	fmt.Println("")
func (m sampleModel) update() model {
	m.progress += 0.01
	return m
}

// +-----------------------++-------------------+
// | This is test program! || Second block here |
// +-----------------------++-------------------+
// [============================================]
func (m sampleModel) view() widgets.Widget {
	return vBox.New(
		hBox.New(
			border.New(str.New("This is test program!")),
			border.New(str.New("Second block here"))),
		progressBar.New(m.progress, 46),
	)
}
