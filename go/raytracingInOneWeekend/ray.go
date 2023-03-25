package main
import "github.com/Cj-bc/playground/raytracingInOneWeekend/vec3"

type Ray struct {
	Origin vec3.Vec3
	Direction vec3.Vec3
}

func Ray(orig Vec3, dir Vec3) Ray {
	return Ray{orig, dir}
}

func (r Ray) At(t float64) vec3.Vec3 {
	return r.Origin.Add(r.Direction.MulScalar(t))
}
