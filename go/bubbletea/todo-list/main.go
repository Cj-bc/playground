package main

import (
	"io"
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

func (t task) View() string { return t.title }
func (t task) Title() string { return t.title }
func (t task) Description() string { return t.description }
func (t task) FilterValue() string { return t.title }

type taskDelegate struct {}

func (delgate taskDelegate) Render(w io.Writer, m list.Model, index int, item list.Item) {
	var task_ task
	
	if item, ok := item.(task); ok {
		task_ = item
	} else {
		return
	}
	if m.Width() <= 0 { return }

	fmt.Fprintf(w, "%s\n%s", task_.title, task_.description)
}

func (delegate taskDelegate) Height() int { return 5 }
func (delegate taskDelegate) Spacing() int { return 1 }
func (delegate taskDelegate) Update(msg tea.Msg, m *list.Model) tea.Cmd {
	return nil
}

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
		task{title: "FooBar", description: "This is dummy description"},
			task{title: "English", description: "English is a language"},
			task{title: "This is absolutely nonsence btw"},
	}, new(taskDelegate), 100, 50)

	tea.NewProgram(model{tasks: testTasks, cursor: 0}).Run()
}
