package vBox

import (
	"fmt"
	"math"
	"example.com/cjbc/termenv/widgetSystem/widgets"
)

type VBox struct {
	width int
	height int
	contents []widgets.Widget
}

func (box VBox) Render() string {
	res := ""
	for _, w := range box.contents {
		res += fmt.Sprint(w.Render(), "\n")
	}
	return res
}

func (box VBox) Width() int { return box.width; }
func (box VBox) Height() int { return box.height; }

func New(contents ...widgets.Widget) VBox {
	width, height := 0.0, 0.0
	for _, w := range contents {
		width = math.Max(width, float64(w.Width()))
		height = math.Max(height, float64(w.Height()))
	}
	return VBox{int(width), int(height), contents}
}
