package main
import (
	"fmt"
)
func deferTest() {
	fmt.Printf("Begining of defer Test\n")
	i := 0
	for i := 0; i < 5; i++ {
		defer fmt.Printf("%d ", i)
	}
	fmt.Printf("End of deferTest. Current i is %v\n", i)
}
func main() {
	deferTest()
}
