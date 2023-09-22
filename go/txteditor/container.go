package main

// Container holds actual text of a buffer.
type Container interface {
	Contents() string
	EndOfLine(point int) (int, error)
	Substring(a, b int) (string, error)
	BeginningOfLine(point int) (int, error)
	GetPointOfIndex(index int) (BufCoord, error)
}


// Coordinate in buffer space.
//
// It is not ready for display as it counts each charcter whereas
// tabs(\t) have TABSTOP width.  convert to ScreenCoord before use
// them.
type BufCoord struct {
	x int
	y int
}
