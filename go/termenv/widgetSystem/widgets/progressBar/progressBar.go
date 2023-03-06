package progressBar

import (
	"math"
	"strings"
	"fmt"
)

type progressBar struct {
	progress float64 // Should be between 0.0 .. 1.0
	width int
	height int
}

func (p progressBar) Render() string {
	limitedP := math.Min(1.0, math.Max(0.0, p.progress))
	length := p.Width() - 2
	completedLen := int(math.Ceil(float64(length) * limitedP))
	return fmt.Sprintf("[%v%v]", strings.Repeat("=", completedLen), strings.Repeat("-", length - completedLen))
}

func (p progressBar) Width() int { return p.width }
func (p progressBar) Height() int { return p.height }

func New(progress float64, width int) progressBar {
	return progressBar{progress, width, 1}
}
