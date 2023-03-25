// Simple program to write gradation PPM image to stdout
package main

import "os"
import "fmt"

func main() {
	width, height := 256.0, 256.0

	file := os.Stdout

	file.WriteString(fmt.Sprintf("P3\n%d %d\n255\n", int(width), int(height)))
	for h := 0.0; h < height; h++ {
		for w := 0.0; w < width; w++ {
			r := w / width * 255
			g := h / height * 255
			file.WriteString(fmt.Sprintf("%d %d %d ", int(r), int(g), 0))
		}
		file.WriteString("\n")
	}
}
