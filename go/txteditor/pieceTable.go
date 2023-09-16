package main

import (
	"fmt"
	"strings"
)

const (
	BufTypeOrigin = iota
	BufTypeAddition
)

const TABSTOP = 8

/// Record represents one piece in PieceTable.
/// It denote which table to use, first index in table, and length of text
type Record struct {
	bufType int
	startIdx int
	length int
}

/// Coordinate in buffer space.
///
/// It is not ready for display as it counts each charcter whereas
/// tabs(\t) have TABSTOP width.  convert to ScreenCoord before use
/// them.
type BufCoord struct {
	x int
	y int
}

type PieceTable struct {
	origin string
	addition string
	records []Record
}

/// Create PieceTable for given file.
func PieceTableFromString(content string) PieceTable {
	return PieceTable {origin:content,
		addition: "",
		records: []Record{Record{bufType: BufTypeOrigin, startIdx: 0, length: len(content)}},
	}
}

/// Returns contents of given PieceTable.
func (pt PieceTable) Contents() string {
	var ret string
	for i := 0; i < len(pt.records); i++ {
		ret += pt.RecordString(pt.records[i])
	}
	return ret
}

func EmptyPieceTable() PieceTable {
	return PieceTable {}
}

func (table PieceTable) RecordString(record Record) string {
	switch record.bufType {
	case BufTypeOrigin:
		return table.origin[record.startIdx:record.startIdx+record.length]
	case BufTypeAddition:
		return table.addition[record.startIdx:record.startIdx+record.length]
	default:
		// This should not happen, so I use panic here.
		panic(fmt.Sprintf("PieceTable.RecordString: Invalid BufType %d", record.bufType))
	}
}

// Find end of line.
// Returns error if point is nagative value, or no more lines are exist
func (table PieceTable) EndOfLine(point int) (int, error) {
	if (point < -1) {
		return 0, fmt.Errorf("Out of range point %d", point)
	}

	// Find record that contains 'point'
	// Also, get substring of that record after 'point'
	var recordIdx int = -1
	var restStr string
	var offset int
	currentLen := 0
	for i := 0; i < len(table.records); i++ {
		if point <= currentLen + table.records[i].length {
			offset = point - currentLen;
			restStr = table.RecordString(table.records[i])[offset:]
			recordIdx = i
		}
		currentLen += table.records[i].length
	}

	// Could not find record that contains given point
	if recordIdx == -1 {
		return 0, fmt.Errorf("Out of range point %d", point)
	}

	if newlineIdx := strings.Index(restStr, "\n"); newlineIdx != -1 {
		// TODO: Is this correct offset? Don't we need +-1?
		return point + newlineIdx, nil
	}

	// Iterate over successors to find first newline
	for i := recordIdx; i < len(table.records); i++ {
		if newlineIdx := strings.Index(table.RecordString(table.records[i]), "\n"); newlineIdx != -1 {
			offset += newlineIdx
			break
		}
		offset += table.records[i].length
	}
	return point + offset, nil
}

// Find beginning of line.
// Returns error if point is nagative value, or no more lines are exist
func (table PieceTable) BeginningOfLine(point int) (int, error) {
	if (point < -1) {
		return 0, fmt.Errorf("Out of range point %d", point)
	}

	// Find record that contains 'point' position
	currentLen := 0
	recordIndex := -1
	var offset int // (point - RECORD_BEGGINING_POINT)
	for i := 0; i < len(table.records); i++ {
		if point <= currentLen + table.records[i].length {
			offset = point - currentLen
			recordIndex = i
			break
		}
		currentLen += table.records[i].length
	}

	// Could not find record that contains 'point'
	if recordIndex == -1 {
		return 0, fmt.Errorf("Out of range point %d", point)
	}

	// if current record have newline before point, return it
	{
		str := table.RecordString(table.records[recordIndex])
		if idx := strings.LastIndex(str[:offset], "\n"); idx != -1 {
			// '+1' because bol is after "\n"
			return currentLen + idx + 1, nil
		}
	}

	// Lookup newline in predecessors
	offsetFromPoint := 0 // current head position relateive to 'point'
	for i := recordIndex - 1; 0 <= i; i-- {
		offsetFromPoint -= table.records[i].length
		if idx := strings.LastIndex(table.RecordString(table.records[i]), "\n"); idx != -1 {
			// '+1' because bol is after "\n"
			return point + offsetFromPoint + idx + 1, nil
		}
	}

	// If it cannot find newline, it shuold be the beginning of the file
	return 0, nil
}

// Returns X-Y coordinate of given index.
// Coordinate origin is at left-top.
func (table PieceTable) GetPointOfIndex(index int) (BufCoord, error) {
	if index < 0 {
		return BufCoord{}, fmt.Errorf("Out of range index %d", index)
	}

	var x int = 0
	var y int = 0
	var currentLength int = 0
	for i := 0; i < len(table.records); i++ {
		// When 'index' is located in currently visiting record
		if index <= (currentLength + table.records[i].length) {
			// Calculate both Y/X coordinate and return
			restLength := index - currentLength
			restString := table.RecordString(table.records[i])[:restLength]
			// "foo\nbar" have two lines, so I need to add 1 to count of "\n"
			y += 1 + strings.Count(restString, "\n")

			if lastBoL := strings.LastIndex(restString, "\n"); lastBoL == -1 {
				// If restString does not contains newlines
				tabs := strings.Count(restString, "\t")
				x = len(restString) + tabs * (TABSTOP - 1)
			} else {
				tabs := strings.Count(restString[lastBoL:], "\t")
				x = len(restString[lastBoL:]) + tabs * (TABSTOP - 1)
			}
			return BufCoord{x: x, y: y}, nil
		} else {
			currentLength += table.records[i].length
			// "foo\nbar" have two lines, so I need to add 1 to count of "\n"
			y += 1 + strings.Count(table.RecordString(table.records[i]), "\n")
		}
	}

	return BufCoord{}, fmt.Errorf("Point out of index. max point is %d", currentLength)
}

// Find record that contains given 'point' and return its index
func (table PieceTable) FindRecordIndex(point int) (index, offset int, err error) {
	if point < 0 {
		return 0, 0, fmt.Errorf("Out of range index %d", point)
	}

	var currentLength int = 0
	for i := 0; i < len(table.records); i++ {
		if point <= (currentLength + table.records[i].length) {
			return i, point - currentLength, nil
		}
		currentLength += table.records[i].length
	}
	return 0, 0, fmt.Errorf("Out of range index %d", point)
}
