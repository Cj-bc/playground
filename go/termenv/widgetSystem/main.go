// Trying termenv
package main

import (
	"github.com/muesli/termenv"
	"fmt"
	"io"
	"unicode/utf8"
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
	debug string
}

func main() {
	run(sampleModel{})
}

func run(m model) {
	o := termenv.DefaultOutput()
	defer o.Reset()
	o.AltScreen()
	defer o.ExitAltScreen()
	state := state{model: sampleModel{}}

	tick := time.Tick(5*time.Millisecond)
	timeout := time.After(5*time.Second)

	keyEvent := make(chan rune)
	go emitKeyEvent(o.TTY(), keyEvent)
	for !state.isEnded {
		select {
		case <-timeout:
			state.isEnded = true
		case <-tick:
			m := state.model.update()
			state.model = m
			o.ClearScreen()
			fmt.Println(state.model.view().Render(), "\n", "debug: ", state.debug)
		case s := <-keyEvent:
			state.debug = "Pressed: " + string(s)
			if s == 'q' {
				state.isEnded = true
			}
		}
	}
}

func emitKeyEvent(fd io.Reader, ch chan rune) {
	var buf [256]byte
	for ;; {
		if _, err := fd.Read(buf[:]); err == nil {
			if r, size := utf8.DecodeRune(buf[:]); size > 1 {
				ch <-r
			}
		}
	}
}

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
