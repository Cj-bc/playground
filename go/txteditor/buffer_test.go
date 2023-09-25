package main

import (
	"testing"
)

var (
	complicatedBuffer = Buffer{
		container: PieceTableFromString("This is Test.\nComplicated Buffer with multiple lines."),
		point: 0, file: nil}
)

func TestBufferPointCoord(t *testing.T) {
	content := "This is test string."
	buf := Buffer{container: PieceTableFromString(content),
		point: 0, file: nil}

	for i := 0; i < len(content); i++ {
		if x, y, err := buf.PointCoord(); err != nil {
			t.Errorf("Unexpected error: %v", err)
		} else if x != i {
			t.Errorf("X coordinate should be %d, but got %d", i, x)
		} else if y != 0 {
			t.Errorf("Y coordinate should be 0, but got %d", y)
		}
		buf.point++
	}
}

func TestBufferPointCoordOneTab(t *testing.T) {
	content := "One\ttab"
	buf := Buffer{container: PieceTableFromString(content),
		point: 0,file: nil, tabstop: 8}

	screenXs := []int{0, 1, 2, 3, 7, 8, 9}
	for i := 0; i < len(content); i++ {
		if x, y, err := buf.PointCoord(); err != nil {
			t.Errorf("Unexpecetd error at i=%d: %v", i, err)
		} else if x != screenXs[i] {
			t.Errorf("X coordinate should be %d, but got %d", screenXs[i], x)
		} else if y != 0 {
			t.Errorf("Y coordinate should be 0, but got %d", y)
		}
		buf.Forward(1)
	}
}

func TestBufferPointCoordDoubleTabs(t *testing.T) {
	content := "Two\t\ttab"
	buf := Buffer{container: PieceTableFromString(content),
		point: 0,file: nil, tabstop: 8}

	screenXs := []int{0, 1, 2, 3, 7, 15, 16, 17}
	for i := 0; i < len(content); i++ {
		if x, y, err := buf.PointCoord(); err != nil {
			t.Errorf("Unexpecetd error at i=%d: %v", i, err)
		} else if x != screenXs[i] {
			t.Errorf("X coordinate should be %d, but got %d", screenXs[i], x)
		} else if y != 0 {
			t.Errorf("Y coordinate should be 0, but got %d", y)
		}
		buf.Forward(1)
	}
}
