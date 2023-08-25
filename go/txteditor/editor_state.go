package main

import (
	"fmt"
)

var defaultKeymap = map[rune]Command {'q': Quit}
type EditorState struct {
	exit bool
	buffers []Buffer
	currentBufferIdx int
	keymap map[rune]Command
}

func EditorStateWithBuffer(buf Buffer) EditorState {
	return EditorState { exit: false,
		buffers: []Buffer{buf},
		currentBufferIdx: 0,
		keymap: defaultKeymap,
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
