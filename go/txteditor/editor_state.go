package main

import (
	"fmt"
)

type EditorState struct {
	exit bool
	buffers []Buffer
	currentBufferIdx int

}

func EditorStateWithBuffer(buf Buffer) EditorState {
	return EditorState { exit: false,
		buffers: []Buffer{buf},
		currentBufferIdx: 0,
	}
}

func EmptyEditorState() EditorState {
	return EditorStateWithBuffer(Buffer{pieceTable: EmptyPieceTable(), point: 0})
}

func (st EditorState) CurrentBuffer() Buffer {
	if len(st.buffers) < st.currentBufferIdx {
		// As this should not occur, I use panic instead of Error
		panic(fmt.Sprintf("currentBufferIdx out-of-range: %d, but limit is %d", st.currentBufferIdx, len(st.buffers)))
	}
	return st.buffers[st.currentBufferIdx]
}
