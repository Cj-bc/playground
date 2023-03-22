// Simple program to write gradation PPM image to stdout
package main

import "os"
import "fmt"

func main() {
	width, height := 256, 256

	file := os.Stdout

	file.WriteString(fmt.Sprintf("P3\n%d %d\n255\n", width, height))
	for h := 0; h < height; h++ {
		for w := 0; w < width; w++ {
			file.WriteString(fmt.Sprintf("%d %d %d ", w, h, 0))
		}
		file.WriteString("\n")
	}
}
