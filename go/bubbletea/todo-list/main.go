package main

import (
	"fmt"
	tea "github.com/charmbracelet/bubbletea"
)

// Task Model {{{
type task struct {
	title string
	isDone bool
	description string
}

type updateMsg func(task) task
type toggleDoneMsg struct{}

func (t task) Init() tea.Cmd {
	return nil
}

func (t task) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg.(type) {
		//case updateMsg:
		//	//return func(task) task(msg)(t), nil
	//	task(msg).title
	case toggleDoneMsg:
		t.isDone = !t.isDone
	}

	return t, nil
}

func (t task) View() string {
	isDone := " "
	if t.isDone { isDone = "x"; }

	return fmt.Sprintf("[%s] %s", isDone, t.title)
}
// }}}

// Model declarations {{{
type model struct {
	tasks []task
	cursor int
}

func (m model) Init() tea.Cmd {
	return nil
}

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "q":
			return m, tea.Quit
		case "j":
			if m.cursor < len(m.tasks)-1 {
				m.cursor++
			}
		case "k":
			if m.cursor > 0 {
				m.cursor--
			}
		case " ":
			updatedTask, cmd := m.tasks[m.cursor].Update(toggleDoneMsg{})
			if task, ok := updatedTask.(task); ok {
				m.tasks[m.cursor] = task
			}
			
			return m, cmd
		}
	}
	return m, nil
}

func (m model) View() string {
	s := ""
	for i, item := range m.tasks {
		cursor := " "
		if i == m.cursor { cursor = ">"; }

		s += fmt.Sprintf("%s %s\n", cursor, item.View())
	}
	return s
}
// }}}

func main() {
	testTasks := []task{
		task{title: "FooBar"},
		task{title: "English"},
		task{title: "This is absolutely nonsence btw"},
	}

	tea.NewProgram(model{tasks: testTasks, cursor: 0}).Run()
}
