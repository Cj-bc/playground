package border

import (
	"strings"
	"fmt"
	"example.com/cjbc/termenv/widgetSystem/widgets"
)

type border struct {
	height int
	width int
	content widgets.Widget
}

func (s border) Render() string {
	topBottom := "+" + strings.Repeat("-", s.content.Width() + 2) + "+"
	middle := "| " + strings.Replace(s.content.Render(), "\n", " |\n| ", -1) + " |"

	return fmt.Sprint(topBottom, "\n", middle, "\n", topBottom)
}

func (s border) Width() int {return s.width}
func (s border) Height() int {return s.height}

func New(w widgets.Widget) border {
	return border{
		width: w.Width() + 4,
		height: w.Height() + 2,
		content: w,
	}
}
