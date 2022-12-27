package main

import (
	"fmt"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/bubbles/list"
)

// Task Model {{{
type task struct {
	title string
	isDone bool
	description string
}

type updateTitleMsg string
type updateDescriptionMsg string
type toggleDoneMsg struct{}

func (t task) Init() tea.Cmd {
	return nil
}

func (t task) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case updateTitleMsg:
		t.title = string(msg)
	case updateDescriptionMsg:
		t.description = string(msg)
	case toggleDoneMsg:
		t.isDone = !t.isDone
	}

	return t, nil
}

func (t task) View() string {
	isDone := " "


	return fmt.Sprintf("[%s] %s", isDone, t.title)
}

func (t task) Title() string { return t.title }
func (t task) Description() string { return t.description }
func (t task) FilterValue() string { return t.title }

// }}}

// Model declarations {{{
type model struct {
	tasks list.Model
	cursor int
}

func (m model) Init() tea.Cmd {
	return nil
}

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	newTasks, cmd := m.tasks.Update(msg)
	m.tasks = newTasks
	return m, cmd
}

func (m model) View() string {
	return m.tasks.View()
}
// }}}

func main() {
	testTasks := list.New([]list.Item {
		task{title: "FooBar"},
		task{title: "English"},
		task{title: "This is absolutely nonsence btw"},
	}, list.NewDefaultDelegate(), 100, 50)

	tea.NewProgram(model{tasks: testTasks, cursor: 0}).Run()
}
