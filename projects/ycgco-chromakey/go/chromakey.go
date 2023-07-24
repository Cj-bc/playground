package main

import "os"
import "io"
import "fmt"
import "math"

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

// YCgCo represents premultiplied-alpha 32-bit YCgCo color.
// bit length is determined because of YCbCr definition.
type YCgCo struct {
	Y uint8
	Cg uint8
	Co uint8
	A uint8
}

func (p YCgCo) RGBA() (r, g, b, a uint32) {
	/*
	   RGBA() は以下の性質を持つ

	   # 1. Premultiplied-Alphaを返す

	   Premultiplied-Alphaとは、アルファ値を事前にRGBに乗算した値である。
	   これは、ブレンド処理の高速化において有意に働く。例えば、xとyをブレンドする時、
	   通常であれば以下のように行う。

	   x.rgb * x.a + y.rgb * (1 - x.a)

	   しかし、乗算はコストのかかる処理であり、そのうち ~x.rgb * x.a~ に関しては「xの値だけで計算出来るのだから、
	   元から計算した値を持っておけばよくない？」という発想が出てきた。それがPremultiplied alpha。

	   premultiplied-alphaなRGBA値を使うと、xとyのブレンドは:

	   x + y.rgb * (1 - x.a)

	   として行われるみたい？

	   ref: http://www.nekomataya.info/nekojyarashi/wiki.cgi?AlphaMode, accessed: [2023-07-24 Mon]
	   ref: https://assistc.hatenablog.jp/entry/premultiplied-alpha, accessed: [2023-07-24 Mon]

	 */

	// y, cg, co := p.To01()
	// r = (y + cg - co) * 65535
	//   = ((p.Y/255) + (p.Cg/255) - (p.Co/255)) * 65535
	//   = ((p.Y + p.Cg - p.Co) / 255) * 65535
	//   = (p.Y + p.Cg - p.Co) * 257
	//   = (p.Y + p.Cg - p.Co) * ((1<<8)-1)
	//   = ((p.Y + p.Cg - p.Co)<<8) - (p.Y + p.Cg - p.Co)
	//   = ((p.Y + p.Cg - p.Co)<<8) - p.Y - p.Cg + p.Co
	r = uint32(((p.Y + p.Cg - p.Co)<<8) - p.Y - p.Cg + p.Co)
	g = uint32(((p.Y        + p.Co)<<8) - p.Y - p.Co)
	b = uint32(((p.Y - p.Cg - p.Co)<<8) - p.Y + p.Cg + p.Co)
	a = p.A

	return
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
			_, err = fmt.Fprintf(file, "%d %d %d\t", p.R, p.G, p.B)
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
			uint8(float64(p.R) * maskVal),
			uint8(float64(p.G) * maskVal),
			uint8(float64(p.B) * maskVal),
		}
	}
	return res
}


func main() {
	file := os.Stdout
	pixs := [250 * 250]RgbPixel{}
	fill(pixs[:], RgbPixel{0, 0, 255}, 250, 0, 100, 0, 100)
	fill(pixs[:], RgbPixel{0, 255, 0}, 250, 100, 200, 0, 100)
	fill(pixs[:], RgbPixel{255, 0, 0}, 250, 200, 250, 0, 100)
	fill(pixs[:], RgbPixel{10, 255, 10}, 250, 100, 200, 100, 200)

	keyed := SampleChromaKey(pixs[:], RgbPixel{0, 120, 0}, 0.2, 1)
	writePPM(file, keyed[:], 250)

}
