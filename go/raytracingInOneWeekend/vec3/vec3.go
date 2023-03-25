package vec3
import (
	"math"
	"io"
	"fmt"
)

type Vec3 struct {
	X float64
	Y float64
	Z float64
}

type Color = Vec3
type Point3 = Vec3

func New(x float64, y float64, z float64) Vec3 {
	return Vec3{x, y, z}
}

func Zero() Vec3 {
	return Vec3{0.0, 0.0, 0.0}
}

func (v Vec3) Length() float64 {
	return math.Sqrt(v.LengthSquared())
}

func (v Vec3) LengthSquared() float64 {
	return v.X*v.X + v.Y*v.Y + v.Z*v.Z
}


func (v1 Vec3) Add(v2 Vec3) Vec3 {
	return Vec3{v1.X + v2.X, v1.Y + v2.Y, v1.Z + v2.Z}
}

func (v1 Vec3) Sub(v2 Vec3) Vec3 {
	return Vec3{v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z}
}

func (v1 Vec3) Mul(v2 Vec3) Vec3 {
	return Vec3{v1.X * v2.X, v1.Y * v2.Y, v1.Z * v2.Z}
}

func (v Vec3) MulScalar(t float64) Vec3 {
	return Vec3{v.X * t, v.Y * t, v.Z * t}
}

func (v Vec3) Div(t float64) Vec3 {
	return v.MulScalar(1/t);
}

func (v1 Vec3) Dot(v2 Vec3) float64 {
	return v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z
}

func (v1 Vec3) Cross(v2 Vec3) Vec3 {
	return Vec3{
		v1.Y * v2.Z - v1.Z * v2.Y,
		v1.Z * v2.X - v1.X * v2.Z,
		v1.X * v2.Y - v1.Y * v2.X,
	}
}

func (v Vec3) UnitVector() Vec3 {
	return v.Div(v.Length())
}

func (v Vec3) Write(writer io.Writer) {
	io.WriteString(writer, fmt.Sprintf("%d %d %d\n", int(v.X), int(v.Y), int(v.Z)))
}

func (color Color) WriteAsColor(writer io.Writer) {
	io.WriteString(writer, fmt.Sprintf("%d %d %d\n", int(255 * color.X),
		int(255 * color.Y), int(255 * color.Z)))
}
