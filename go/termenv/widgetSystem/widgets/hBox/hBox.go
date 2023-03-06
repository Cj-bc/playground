package hBox

import (
	"math"
	"strings"
	"example.com/cjbc/termenv/widgetSystem/widgets"
)

type hBox struct {
	width int
	height int
	contents []widgets.Widget
}

func (box hBox) Render() string {
	widgets := make([][]string, len(box.contents))
	for i, w := range box.contents {
		widgets[i] = strings.Split(w.Render(), "\n")
	}

	res := ""
	for i := 0; i < box.Height(); i++ {
		for j := 0; j < len(widgets); j++ {
			res += widgets[j][i]
		}
		res += "\n"
	}
	res = strings.TrimSuffix(res, "\n")
	return res
}

func (box hBox) Width() int { return box.width; }
func (box hBox) Height() int { return box.height; }

func New(contents ...widgets.Widget) hBox {
	width, height := 0.0, 0.0
	for _, w := range contents {
		width = math.Max(width, float64(w.Width()))
		height = math.Max(height, float64(w.Height()))
	}
	return hBox{int(width), int(height), contents}
}
