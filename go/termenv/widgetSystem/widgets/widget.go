package widgets

type Widget interface {
	Render() string
	Width() int
	Height() int
}

