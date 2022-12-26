package main

import (
	"fmt"

	tea "github.com/charmbracelet/bubbletea"
)

type model struct {
	choices []string
	cursor  int
	selected map[int]struct{}
}

func initialModel() model {
	return model{
		choices: []string{"Complete Go tutorial", "Try bubbletea"},
		selected: make(map[int]struct{}),
	}
}

func (m model) Init() tea.Cmd {
	return nil
}

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "ctrl+c", "q":
			return m, tea.Quit

		case "k":
			if m.cursor > 0 {
				m.cursor--
			}

		case "j":
			if m.cursor < len(m.choices)-1 {
				m.cursor++
			}

		case " ", "enter":
			if _, ok := m.selected[m.cursor]; ok {
				delete(m.selected, m.cursor)
			} else {
				m.selected[m.cursor] = struct{}{}
			}
		}

	}
	return m, nil
}

func (m model) View() string {
	s := "Header line\n"

	for i,item := range m.choices {
		cursor := " "
		selected := " "

		if _, ok := m.selected[i]; ok {
			// It's selected
			selected = "x"
		}
		if i == m.cursor {
			cursor = ">"
		}

		s += fmt.Sprintf("%s [%s] %s\n", cursor, selected, item)
	}
	return s
}


func main() {
	tea.NewProgram(initialModel()).Run()
}
