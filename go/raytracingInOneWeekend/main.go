// Simple program to write gradation PPM image to stdout
package main

import "os"
import "fmt"
import "github.com/Cj-bc/playground/raytracingInOneWeekend/vec3"

func main() {
	width, height := 256.0, 256.0

	file := os.Stdout

	file.WriteString(fmt.Sprintf("P3\n%d %d\n255\n", int(width), int(height)))
	for h := 0.0; h < height; h++ {
		os.Stderr.WriteString(fmt.Sprintf("\rScanlines remaining: %d", int(height-h)))
		for w := 0.0; w < width; w++ {
			color := vec3.New(w/width, h/height, 0.0)
			color.WriteAsColor(file)
		}
		file.WriteString("\n")
	}
	os.Stderr.WriteString("\nDone.\n")
}
