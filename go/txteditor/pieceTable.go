package main

import (
	"os"
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
func PieceTableFromFile(fn string) (PieceTable, error) {
	data, err := os.ReadFile(fn)
	if err != nil {
		return PieceTable{}, err
	}

	return PieceTable {origin:string(data),
		addition: "",
		records: []Record{Record{bufType: BufTypeOrigin, startIdx: 0, length: len(data)}},
	}, nil
}

/// Returns contents of given PieceTable.
func (pt PieceTable) Contents() string {
	var ret string
	for i := 0; i < len(pt.records); i++ {
		record := pt.records[i]
		switch record.bufType {
		case BufTypeOrigin:
			ret += pt.origin[record.startIdx:record.startIdx+record.length]
		case BufTypeAddition:
			ret += pt.addition[record.startIdx:record.startIdx+record.length]
		}
	}
	return ret
}

func EmptyPieceTable() PieceTable {
	return PieceTable {}
}
