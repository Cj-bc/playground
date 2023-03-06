package str

type str string

func (s str) Render() string {return string(s)}
func (s str) Width() int {return len(s)}
func (s str) Height() int {return 1}
func New(s string) str {return str(s)}
