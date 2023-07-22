package main

import "os"
import "io"
import "fmt"

type RgbPixel struct {
	R uint8
	G uint8
	B uint8
}

func (p RgbPixel) To01() (float64, float64, float64) {
	return float64(p.R) / 255.0, float64(p.G) / 255.0, float64(p.B) / 255.0
}

type YcgcoPixel struct {
	Y uint8
	Cg uint8
	Co uint8
}

func (p YcgcoPixel) To01() (float64, float64, float64) {
	return float64(p.Y) / 255.0, float64(p.Cg) / 255.0, float64(p.Co) / 255.0
}

func (p RgbPixel) ToYCgCo() YcgcoPixel {
	r, g, b := p.To01()
	return YcgcoPixel{
		uint8(( r / 4 + g / 2 + b / 4) * 255),
		uint8(( r / 2         - b / 2) * 255),
		uint8((-r / 4 + g / 2 - b / 4) * 255),
	}
}

func (p YcgcoPixel) ToRgb() RgbPixel {
	y, cg, co := p.To01()
	return RgbPixel{
		uint8((y + cg - co) * 255),
		uint8((y      + co) * 255),
		uint8((y - cg - co) * 255),
	}
}

/// Write given vector 'img' to file 'file'
func writePPM(file io.Writer, img []RgbPixel, width uint) error {
	if width < 1 {
		return fmt.Errorf("Width should be more than 1")
	}

	height := uint(len(img) / int(width))
	_, err := fmt.Fprintf(file, "P3\n%d %d\n255\n", width, height)
	if err != nil {
		return fmt.Errorf("Failed to write to file: %w", err)
	}

	for h := uint(0); h < height; h++ {
		for w := uint(0); w < width; w++ {
			p :=  img[h*width + w]
			_, err = fmt.Fprintf(file, "%d %d %d\t", p.R, p.B, p.G)
			if err != nil {
				return fmt.Errorf("Failed to write to file: %w", err)
			}
		}
		fmt.Fprint(file, "\n")
	}

	return nil
}


/// Fill given buffer with given pixel
///
/// top, bottom, left, right are exclusive. That means, they won't be in
func fill(buf []RgbPixel, col RgbPixel, width, top, bottom, left, right uint) {
	for w := left; w < right; w++ {
		for h := top; h < bottom; h++ {
			buf[width * h + w] = col 
		}
	}
}

/// Equivalent to HLSL's smoothstep
///
/// https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-smoothstep
func smoothstep(min, max, x float64) float64 {
	switch {
	case x < min: return 0.0
	case max < x: return 1.0
	default: return (x - min) / (max - min)
	}
}

/// Create chroma key mask for given 'RgbPixel'
func YcgcoMask(pix, key YcgcoPixel, _range, fuzziness float64) float64 {
	_, pixCg, pixCo := pix.To01()
	_, keyCg, keyCo := key.To01()
	pixelChromaDistance := math.Sqrt(math.Pow(pixCg - keyCg, 2) + math.Pow(pixCo - keyCo, 2)) * 10
	return smoothstep(_range, _range + fuzziness, pixelChromaDistance)
}

func SampleChromaKey(tex []RgbPixel, key RgbPixel, _range, fuzziness float64) []RgbPixel {
	res := make([]RgbPixel, len(tex))
	for i := 0; i < len(tex); i++  {
		p := tex[i]
		maskVal := YcgcoMask(p.ToYCgCo(), key.ToYCgCo(), _range, fuzziness)
		res[i] = RgbPixel{
			uint8(float64(p.R) * (1 - maskVal)),
			uint8(float64(p.G) * (1 - maskVal)),
			uint8(float64(p.B) * (1 - maskVal)),
		}
	}
	return res
}


func main() {
	file := os.Stdout
	pixs := [250 * 250]RgbPixel{}
	fill(pixs[:], RgbPixel{0, 0, 255}, 250, 0, 100, 0, 100)
	fill(pixs[:], RgbPixel{0, 255, 0}, 250, 100, 200, 0, 100)

	writePPM(file, pixs[:3], 250)
	fmt.Print(RgbPixel{0, 128, 0}.ToYCgCo())
	fmt.Print(RgbPixel{0, 19, 0}.ToYCgCo().ToRgb())

}
