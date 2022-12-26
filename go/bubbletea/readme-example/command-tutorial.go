package main

import (
	"fmt"
	"net/http"
	"time"
	tea "github.com/charmbracelet/bubbletea"
)

const url = "http://charm.sh/"

type model struct {
	status int
	err error
}

func checkServer() tea.Msg {
	c := &http.Client{Timeout: 10 * time.Second}
	res,err := c.Get(url)

	if err != nil {
		return errMsg{err}
	}

	return statusMsg(res.StatusCode)
}

type statusMsg int
type errMsg struct{err error}

func (e errMsg) Error() string {
	return e.err.Error()
}

func (m model) Init() tea.Cmd {
	return checkServer
}

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case statusMsg:
		m.status = int(msg)
		return m, tea.Quit

	case errMsg:
		m.err = msg
		return m, tea.Quit

	case tea.KeyMsg:
		if msg.Type == tea.KeyCtrlC || msg.String() == "q" {
			return m, tea.Quit
		}
	}

	return m, nil
}

func (m model) View() string {
	if m.err != nil {
		return fmt.Sprintf("An error occured, %s\n\n", m.err)
	}

	s := fmt.Sprintf("Checking %v...\n", url)

	if (m.status > 0) {
		s += fmt.Sprint(m.status, "\n")
	}

	return "\n" + s + "\n\n"
}

func main() {
	tea.NewProgram(model{}).Run()
}
