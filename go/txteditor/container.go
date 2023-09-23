package main

// Container holds actual text of a buffer.
type Container interface {
	// Return contents of this Container
	Contents() string

	// Find end of line and returns that point.
	// Returns error if point is nagative value, or no more lines are exist.
	EndOfLine(point int) (int, error)

	// Return substring of given indices.
	// index a, and b are included in the result.
	// As both a and b are index, it should be between 0 to (MAX_LEN - 1)
	//
	// e.g. In order to retrive all contents:
	//
	//     Container.Substring(0, MAX_LEN - 1)
	Substring(a, b int) (string, error)

	// Find beginning of line and returns that point.
	// Returns error if point is nagative value, or no more lines are exist
	BeginningOfLine(point int) (int, error)

	// Returns X-Y buffer coordinate of given index.
	// Coordinate origin is at left-top.
	// This isn't ready to use as display coordinate.
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
