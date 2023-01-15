package main

import (
	"math"
	"time"
	"math/rand"
	"fmt"
)

// 半径1の円と、それに外接する一辺2の正方形を考える。この正方形上に
// ランダムに点を落とした場合、円の中に入る点と入らない点と出てくる。
// 全ての点の数と、円の中に落ちる点の数の比率は正方形と円の面積比と等
// しくなると考えられるので、円の面積の式に出てくる円周率を計算する。
func main() {
	rand.Seed(time.Now().UnixNano())
	N := 10000000
	
	n := 0
	for i := 0; i < N; i++ {
		x, y := rand.Float64(), rand.Float64()
		if math.Sqrt(math.Pow(x, 2) + math.Pow(y, 2)) < 1.0 {
			n++
		}
	}

	fmt.Println("N: ", N)
	fmt.Println("n: ", n)
	// 円の面積: $$r^2 * pi = pi$$
	// 半径1の円を考えているため、円の面積は素直にpi
	// 正方形の面積: $$2^2 = 4$$
	// N:n = 4:pi
	// 4n = N*pi
	// pi = 4n/N
	fmt.Println("Pi: ", 4*float64(n)/float64(N))
}
