package main


type Command struct {
	Exec func(st EditorState) EditorState
}

var Quit = Command { Exec: func(st EditorState) EditorState { st.exit = true; return st }}

var Forward = Command { Exec: func(st EditorState) EditorState {
	buf := st.CurrentBuffer()
	buf.point += 1
	return st
}}

var Backward = Command { Exec: func(st EditorState) EditorState {
	buf := st.CurrentBuffer()
	buf.point -= 1
	return st
}}
