package main

import (
	"os"
	"fmt"
	"strings"
)

// Default value of tabstop
const DEFUALT_TABSTOP = 8

/// You should call 'Buffer.Close()'
type Buffer struct {
	container PieceTable
	point int /// Cursor point
	file *os.File
	tabstop int
}

func NewFileBuffer(fn string) (Buffer, error) {
	f, err := os.OpenFile(fn, os.O_RDWR | os.O_CREATE, 0755)
	if err != nil {
		return Buffer{}, err
	}

	// Read contents
	var fSize int64
	if fStat, err := f.Stat(); err != nil {
		return Buffer{}, fmt.Errorf("Failed to read status of file(%s). %w", fn, err)
	} else {
		fSize = fStat.Size()
	}

	buf := make([]byte, fSize)
	_, err = f.Read(buf)
	if err != nil {
		return Buffer{}, fmt.Errorf("Failed to read content of file(%s). %w", fn, err)
	}
	// TODO: Do something if it could not read all

	return Buffer{container: PieceTableFromString(string(buf)),
		point: 0, file: f, tabstop: DEFUALT_TABSTOP}, nil
}

func EmptyBuffer() Buffer {
	return Buffer{container: EmptyPieceTable(), point: 0, tabstop: DEFUALT_TABSTOP}
}

func (buf Buffer) Contents() string {
	return buf.container.Contents()
}

func (buf Buffer) Close() {
	buf.file.Close()
}

// Returns point's coordinate in terminal.
// Coordinate start at (0,0) at top left corner,
func (buf Buffer) PointCoord() (int, int, error) {
	c, _ := buf.container.GetPointOfIndex(buf.point)

	bol, err := buf.container.BeginningOfLine(buf.point)
	if err != nil {
		return 0, 0, fmt.Errorf("Invalid buffer point '%d': %w", buf.point, err)
	}

	// Make sure bol < buf.point - 1 in following procedures
	if bol == buf.point {
		return 0, c.y, nil
	}

	subStr, err := buf.container.Substring(bol, buf.point - 1)
	if err != nil {
		return 0, 0, fmt.Errorf("Invalid buffer point '%d': %w", buf.point, err)
	}
	
	return c.x + (strings.Count(subStr, "\t") * 7), c.y, nil
}

func (buf *Buffer) Forward(n int) error {
	// Testing if it won't be out-of-range
	// TODO: it might be better to measure length.
	if _, err := buf.container.GetPointOfIndex(buf.point+n); err != nil {
		return fmt.Errorf("Reached end of buffer")
	}

	buf.point += n
	return nil
}

func (buf *Buffer) EndOfLine() error {
	eolPoint, err := buf.container.EndOfLine(buf.point)
	if err != nil {
		return err
	}

	buf.point = eolPoint
	return nil
}

func (buf *Buffer) BeginningOfLine() error {
	bolPoint, err := buf.container.BeginningOfLine(buf.point)
	if err != nil {
		return err
	}

	buf.point = bolPoint
	return nil
}

// Move to the same column of the next line 
func (buf *Buffer) NextLine() error {
	c, err := buf.container.GetPointOfIndex(buf.point)
	if err != nil {
		return err
	}

	eolpoint, err := buf.container.EndOfLine(buf.point)
	if err != nil {
		return err
	}

	// Make sure buf.point is on the next line without
	nextLineEol, err := buf.container.EndOfLine(eolpoint+1)
	if err != nil {
		return err
	}
	buf.point = eolpoint + c.x
	if (nextLineEol - eolpoint) < c.x {
		buf.point = nextLineEol
	}

	return nil
}
