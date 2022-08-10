#include <cairo.h>

int main(int argc, char *argv[]) {
	cairo_surface_t *surface;
	cairo_t *cr;
	cairo_pattern_t *radpad, *linpat;

	surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 120, 120);
	cr = cairo_create (surface);

	cairo_scale (cr, 120, 120);

	radpad = cairo_pattern_create_radial(0.25, 0.25, 0.1, 0.5, 0.5, 0.5);
	cairo_pattern_add_color_stop_rgb(radpad, 0, 0.0, 1.0, 0.8);
	cairo_pattern_add_color_stop_rgb(radpad, 1, 0.9, 0.0, 0.0);

	cairo_rectangle(cr, 0.0, 0.0, 0.3, 0.3);
	cairo_set_source(cr, radpad);
	cairo_fill(cr);

	cairo_surface_write_to_png (surface, "setsourcegradient.png");
	cairo_destroy(cr);
	cairo_surface_destroy(surface);
	return 0;

}
