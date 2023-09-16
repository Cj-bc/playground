package main

import (
	"os"
	"fmt"
)

/// You should call 'Buffer.Close()'
type Buffer struct {
	pieceTable PieceTable
	point int /// Cursor point
	file *os.File
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

	return Buffer{pieceTable: PieceTableFromString(string(buf)),
		point: 0, file: f}, nil
}

func EmptyBuffer() Buffer {
	return Buffer{pieceTable: EmptyPieceTable(), point: 0}
}

func (buf Buffer) Contents() string {
	return buf.pieceTable.Contents()
}

func (buf Buffer) Close() {
	buf.file.Close()
}

// Returns point's coordinate in terminal
// FIXME: THIS IS BROKEN
func (buf Buffer) PointCoord() (int, int, error) {
	c, _ := buf.pieceTable.GetPointOfIndex(buf.point)

	bol, err := buf.pieceTable.BeginningOfLine(buf.point)
	if err != nil {
		return 0, 0, fmt.Errorf("Invalid buffer point '%d': %w", buf.point, err)
	}

	subStr, err := buf.pieceTable.Substring(bol, buf.point)
	if err != nil {
		return 0, 0, fmt.Errorf("Invalid buffer point '%d': %w", buf.point, err)
	}

	return c.x, c.y, nil
}

func (buf *Buffer) Forward(n int) error {
	// Testing if it won't be out-of-range
	// TODO: it might be better to measure length.
	if _, err := buf.pieceTable.GetPointOfIndex(buf.point+n); err != nil {
		return fmt.Errorf("Reached end of buffer")
	}

	buf.point += n
	return nil
}

func (buf *Buffer) EndOfLine() error {
	eolPoint, err := buf.pieceTable.EndOfLine(buf.point)
	if err != nil {
		return err
	}

	buf.point = eolPoint
	return nil
}

func (buf *Buffer) BeginningOfLine() error {
	bolPoint, err := buf.pieceTable.BeginningOfLine(buf.point)
	if err != nil {
		return err
	}

	buf.point = bolPoint
	return nil
}

func (buf *Buffer) NextLine() error {
	c, err := buf.pieceTable.GetPointOfIndex(buf.point)
	if err != nil {
		return err
	}

	eolpoint, err := buf.pieceTable.EndOfLine(buf.point)
	if err != nil {
		return err
	}

	buf.point = eolpoint + 1 + c.x
	return nil
}
