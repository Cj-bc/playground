package main

import (
	"fmt"
	"strings"
)

const (
	BufTypeOrigin = iota
	BufTypeAddition
)

/// Record represents one piece in PieceTable.
/// It denote which table to use, first index in table, and length of text
type Record struct {
	bufType int
	startIdx int
	length int
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

// Returns X-Y coordinate of given index.
// Coordinate origin is at left-top.
func (table PieceTable) GetPointOfIndex(index int) (int, int, error) {
	var x int
	var y int
	var currentLength int = 0
	for i := 0; i < len(table.records); i++ {
		// When 'index' is located in currently visiting record
		if index < (currentLength + table.records[i].length) {
			// Calculate both Y/X coordinate and return
			restLength := index - currentLength
			restString := table.RecordString(table.records[i])[:restLength]
			// "foo\nbar" have two lines, so I need to add 1 to count of "\n"
			y += 1 + strings.Count(restString, "\n")

			if lastBoL := strings.LastIndex(restString, "\n"); lastBoL == -1 {
				// If restString does not contains newlines
				x = len(restString)
			} else {
				x = len(restString[lastBoL:])
			}
			return x, y, nil
		} else {
			currentLength += table.records[i].length
			// "foo\nbar" have two lines, so I need to add 1 to count of "\n"
			y += 1 + strings.Count(table.RecordString(table.records[i]), "\n")
		}
	}

	return 0, 0, fmt.Errorf("Point out of index. max point is %d", currentLength)
}
