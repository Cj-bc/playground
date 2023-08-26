package main

import (
	"fmt"
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
