package main

type Ray struct {
	Origin Vec3
	Direction Vec3
}

func Ray(orig Vec3, dir Vec3) Ray {
	return Ray{orig, dir}
}

func (r Ray) At(t float64) {
	return r.Origin + Direction.MulScalar(t)
}
