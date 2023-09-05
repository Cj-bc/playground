package main


type Command struct {
	Exec func(st EditorState) EditorState
}

var Quit = Command { Exec: func(st EditorState) EditorState { st.exit = true; return st }}

var Forward = Command { Exec: func(st EditorState) EditorState {
	buf := st.CurrentBuffer()
	err := buf.Forward(1)
	if err != nil {
		st.errors = append(st.errors, err)
	}
	return st
}}

var Backward = Command { Exec: func(st EditorState) EditorState {
	buf := st.CurrentBuffer()
	err := buf.Forward(-1)
	if err != nil {
		st.errors = append(st.errors, err)
	}
	return st
}}

var EndOfLine = Command { Exec: func(st EditorState) EditorState {
	buf := st.CurrentBuffer()
	err := buf.EndOfLine()
	if err != nil {
		st.errors = append(st.errors, err)
	}
	return st
}}

var BeginningOfLine = Command { Exec: func(st EditorState) EditorState {
	buf := st.CurrentBuffer()
	err := buf.BeginningOfLine()
	if err != nil {
		st.errors = append(st.errors, err)
	}
	return st
}}

var NextLine = Command { Exec: func(st EditorState) EditorState {
	buf := st.CurrentBuffer()
	err := buf.NextLine()
	if err != nil {
		st.errors = append(st.errors, err)
	}

	return st
}}
