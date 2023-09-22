package main

import (
	"fmt"
)

var defaultKeymap = map[rune]Command {'q': Quit, 'h': Backward, 'l': Forward, 'j': NextLine, '$': EndOfLine, '0': BeginningOfLine}
type EditorState struct {
	exit bool
	buffers []Buffer
	currentBufferIdx int
	keymap map[rune]Command
	errors []error
}

func EditorStateWithBuffer(buf Buffer) EditorState {
	return EditorState { exit: false,
		buffers: []Buffer{buf},
		currentBufferIdx: 0,
		keymap: defaultKeymap,
		errors: []error{},
	}
}

func EmptyEditorState() EditorState {
	return EditorStateWithBuffer(EmptyBuffer())
}

func (st EditorState) CurrentBuffer() *Buffer {
	if len(st.buffers) < st.currentBufferIdx {
		// As this should not occur, I use panic instead of Error
		panic(fmt.Sprintf("currentBufferIdx out-of-range: %d, but limit is %d", st.currentBufferIdx, len(st.buffers)))
	}
	return &st.buffers[st.currentBufferIdx]
}
