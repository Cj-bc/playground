// Simple program to write gradation PPM image to stdout
package main

import "os"
import "fmt"
import "github.com/Cj-bc/playground/raytracingInOneWeekend/vec3"

func rayColor(r Ray) vec3.Color {
	unitDir := r.Direction.UnitVector()
	t := 0.5*(unitDir.Y + 1.0) // -1.0~1.0 -> 0.0~1.0
	return vec3.New(1.0, 1.0, 1.0).MulScalar(1-t).Add(vec3.New(0.5, 0.7, 1.0).MulScalar(t))
}

func main() {

	// Image
	aspectRatio := 16.0 / 9.0
	width := 400.0
	height := width / aspectRatio

	// Camera
	viewPortHeight := 2.0
	viewPortWidth := aspectRatio * viewPortHeight
	focalLength := 1.0

	origin := vec3.New(0.0, 0.0, 0.0)
	horizontal := vec3.New(viewPortWidth, 0.0, 0.0)
	vertical := vec3.New(0.0, viewPortHeight, 0.0)
	lowerLeftCorner := origin.Sub(horizontal.Div(2)).
		Sub(vertical.Div(2)).
		Sub(vec3.New(0.0, 0.0, focalLength))

	file := os.Stdout

	file.WriteString(fmt.Sprintf("P3\n%d %d\n255\n", int(width), int(height)))
	for h := 0.0; h < height; h++ {
		os.Stderr.WriteString(fmt.Sprintf("\rScanlines remaining: %d", int(height-h)))
		for w := 0.0; w < width; w++ {
			u := w/(width-1)
			v := h/(height-1)
			r := Ray{origin, lowerLeftCorner.Add(horizontal.MulScalar(u)).Add(vertical.MulScalar(v).Sub(origin))}
			color := rayColor(r)
			color.WriteAsColor(file)
		}
		file.WriteString("\n")
	}
	os.Stderr.WriteString("\nDone.\n")
}
