package main

import (
	"testing"
	"strings"
)

func TestPieceTableFromString(t *testing.T) {
	testTarget := PieceTableFromString("this is a test table")
	ideal := PieceTable{origin: "this is a test table",
		addition: "",
		records: []Record{Record{bufType: BufTypeOrigin,
			startIdx: 0,
			length: 20,
		}}}

	if testTarget.origin != ideal.origin {
		t.Errorf("Origin buffer should be \"%s\", but got \"%s\"", ideal.origin, testTarget.origin)
	}

	if addition := testTarget.addition; addition != "" {
		t.Errorf("Addition buffer should be empty, but got %s", addition)
	}

	if l := len(testTarget.records); l != 1 {
		t.Errorf("It should contains one record, but got %d records", l)
	}

	if r := testTarget.records[0]; r.bufType != BufTypeOrigin {
		t.Errorf("Initial record shuold point to origin, but got %v", r.bufType)
	}
}

func TestPieceTableFromStringContentsCorrect(t *testing.T) {
	content := "this is a test table"
	table := PieceTableFromString(content)
	if c := table.Contents(); c != content {
		t.Errorf("It shuold be \"%s\", but got \"%s\"", content, c)
	}
}

func TestContentsMultipleRecords(t *testing.T) {
	content := "this was an one table record, but anymore"
	table := PieceTable{origin: "this is a test table",
		addition: "wanone record, but anymore",
		records: []Record{
			Record{bufType: BufTypeOrigin, startIdx: 0, length: len("this ")},
			Record{bufType: BufTypeAddition, startIdx: 0, length: len("wa")},
			Record{bufType: BufTypeOrigin, startIdx: len("this i"), length: len("s a")},
			Record{bufType: BufTypeAddition, startIdx: len("wa"), length: len("n")},
			Record{bufType: BufTypeOrigin, startIdx: len("this is a"), length: len(" ")},
			Record{bufType: BufTypeAddition, startIdx: len("wan"), length: len("one")},
			Record{bufType: BufTypeOrigin, startIdx: len("this is a test"), length: len(" table")},
			Record{bufType: BufTypeAddition, startIdx: len("wanone"), length: len(" record, but anymore")},
		}}

	if c := table.Contents(); c != content {
		t.Errorf("Expected \"%s\", but got \"%s\"", content, c)
	}
}

func TestEndOfLine(t *testing.T) {
	table := PieceTableFromString("This is a test text.\nIt contains multiple lines.\nHi there!")
	eols := []int{len("This is a test text."), len("This is a test text.\nIt contains multiple lines.")}

	if eol, err := table.EndOfLine(1); err != nil {
		t.Errorf("Did not expect error, but got %v", err)
	} else {
		if eol != eols[0] {
			t.Errorf("Expected EoL is %d, but got %d", eols[0], eol)
		}
	}

	if eol, err := table.EndOfLine(eols[0]+1); err != nil {
		t.Errorf("Did not expect error, but got %v", err)
	} else {
		if eol != eols[1] {
			t.Errorf("Expected EoL is %d, but got %d", eols[1], eol)
		}
	}
}

func TestBeginningOfLine(t *testing.T) {
	line1 := "This is a test text."
	line2 := "It contains multiple lines."
	line3 := "Hi there!"
	table := PieceTableFromString(strings.Join([]string{line1, line2, line3}, "\n"))

	correctBol := 0
	for i := 0; i < len(line1); i++ {
		if bol, err := table.BeginningOfLine(i); err != nil {
			t.Errorf("Did not expected error at point '%d' but got: %v", i, err)
		} else {
			if bol != correctBol {
				t.Errorf("point %d's beggining of line is %d, but got %d", i, correctBol, bol)
			}
		}
	}

	// In pieceTable, "\n" is added after "line1". So I added 1
	correctBol = len(line1) + 1
	for i := correctBol; i < len(line2); i++ {
		if bol, err := table.BeginningOfLine(i); err != nil {
			t.Errorf("Did not expected error at point '%d' but got: %v", i, err)
		} else {
			if bol != correctBol {
				t.Errorf("point %d's beginning of line is %d, but got %d", i, correctBol, bol)
			}
		}
	}

	correctBol = len(line1) + len(line2) + 2
	for i := correctBol; i < len(line3); i++ {
		if bol, err := table.BeginningOfLine(i); err != nil {
			t.Errorf("Did not expected error at point '%d' but got: %v", i, err)
		} else {
			if bol != correctBol {
				t.Errorf("point %d's beginning of line is %d, but got %d", i, correctBol, bol)
			}
		}
	}
}

func TestPieceTableFindRecordIndexOutOfRange(t *testing.T) {
	table := PieceTable{origin: "this is a test table",
		addition: "wanone record, but anymore",
		records: []Record{
			Record{bufType: BufTypeOrigin, startIdx: 0, length: len("this ")},
			Record{bufType: BufTypeAddition, startIdx: 0, length: len("wa")},
			Record{bufType: BufTypeOrigin, startIdx: len("this i"), length: len("s a")},
			Record{bufType: BufTypeAddition, startIdx: len("wa"), length: len("n")},
			Record{bufType: BufTypeOrigin, startIdx: len("this is a"), length: len(" ")},
			Record{bufType: BufTypeAddition, startIdx: len("wan"), length: len("one")},
			Record{bufType: BufTypeOrigin, startIdx: len("this is a test"), length: len(" table")},
			Record{bufType: BufTypeAddition, startIdx: len("wanone"), length: len(" record, but anymore")},
		}}

	if _, _, err := table.FindRecordIndex(-1); err == nil {
		t.Errorf("'-1' should be out-of-range, but it did not return error")
	}

	if _, _, err := table.FindRecordIndex(10000); err == nil {
		t.Errorf("'10000' should be out-of-range, but it did not return error")
	}
}

func TestPieceTableFindRecordIndex(t *testing.T) {
	table := PieceTable{origin: "this is a test table",
		addition: "wanone record, but anymore",
		records: []Record{
			Record{bufType: BufTypeOrigin, startIdx: 0, length: len("this ")},
			Record{bufType: BufTypeAddition, startIdx: 0, length: len("wa")},
			Record{bufType: BufTypeOrigin, startIdx: len("this i"), length: len("s a")},
			Record{bufType: BufTypeAddition, startIdx: len("wa"), length: len("n")},
			Record{bufType: BufTypeOrigin, startIdx: len("this is a"), length: len(" ")},
			Record{bufType: BufTypeAddition, startIdx: len("wan"), length: len("one")},
			Record{bufType: BufTypeOrigin, startIdx: len("this is a test"), length: len(" table")},
			Record{bufType: BufTypeAddition, startIdx: len("wanone"), length: len(" record, but anymore")},
		}}

	i := 0
	for expectedIdx := 0; expectedIdx < len(table.records); expectedIdx++ {
		length := table.records[expectedIdx].length
		for ; i < length; i++ {
			if idx, _, _ := table.FindRecordIndex(i); idx != expectedIdx {
				t.Errorf("point %d should be in record idx %d, but got %d", i, expectedIdx, idx)
			}
		}
		i += length
	}
}

/// Test PieceTable.Substring() with single record
func TestPieceTableSubstringOneRecord(t *testing.T) {
	str := "This is test text"
	table := PieceTableFromString(str)
	if substr, err := table.Substring(0, len(str) - 1); err != nil {
		t.Errorf("Unexpected error: %v", err)
	} else if (substr != str) {
		t.Errorf("Expected \"%s\", but got \"%s\"", str, substr)
	}
}


/// Test PieceTable.Substring() with multiple records
func TestPieceTableSubstringMultipleRecord(t *testing.T) {
	line1, line2 := "This is ", "test text"
	table := PieceTable{origin: line1+line2, addition: "",
		records: []Record {
			Record{bufType: BufTypeOrigin, startIdx: 0, length: len(line1)},
			Record{bufType: BufTypeOrigin, startIdx: len(line1)+1, length: len(line2)},
		}}

	if substr, err := table.Substring(0, len(line1+line2) - 1); err != nil {
		t.Errorf("Unexpected error: %v", err)
	} else if (substr != line1+line2) {
		t.Errorf("Expected \"%s\", but got \"%s\"", line1+line2, substr)
	}
}

func TestAddRecord(t *testing.T) {
	table := PieceTable{origin: "This is test text buffer",
		addition: "with addition",
		records: []Record{},
	}

	if err := table.AddRecord(BufTypeOrigin, 0, len("This is test text ")); err != nil {
		t.Errorf("Unexpected error: %v", err)
	}
}
