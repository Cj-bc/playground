#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <cairo.h>
#include <cairo-xlib.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/* Most of codes here are borrowed from:
   https://www.cairographics.org/Xlib/
   original copyright notice is below:
*/

/*! Cairo graphics and X11/Xlib motion example. [MODIFIED]
 * @author Bernhard R. Fischer, 2048R/5C5FFD47 <bf@abenteuerland.at>.
 * @version 2014110801
 * Compile with gcc -Wall $(pkg-config --cflags --libs cairo x11) -o cairo_xlib cairo_xlib.c
 */

/*! Check for Xlib Mouse/Keypress events. All other events are discarded. 
 * @param sfc Pointer to Xlib surface.
 * @param block If block is set to 0, this function always returns immediately
 * and does not block. if set to a non-zero value, the function will block
 * until the next event is received.
 * @return The function returns 0 if no event occured (and block is set). A
 * positive value indicates that a key was pressed and the X11 key symbol as
 * defined in <X11/KeySymdef.h> is returned. A negative value indicates a mouse
 * button event. -1 is button 1 (left button), -2 is the middle button, and -3
 * the right button.
 */
int cairo_check_event (cairo_surface_t *sfc, int block) {
  char keybuf[8];
  KeySym key;
  XEvent e;

  for (;;) {
    if (block || XPending (cairo_xlib_surface_get_display(sfc)))
      XNextEvent (cairo_xlib_surface_get_display(sfc), &e);
    else
      return 0;

    switch (e.type) {
        case ButtonPress:
	  return -e.xbutton.button;
        case KeyPress:
	  XLookupString(&e.xkey, keybuf, sizeof (keybuf), &key, NULL);
	  return key;
        default:
	  fprintf(stderr, "Dropping unhandled XEvent.type = %d.\n", e.type);
    }
  }
}

void cairo_get_resize_event (cairo_surface_t *sfc, int *w, int *h) {
  XEvent e;

  for (;;) {
    if (XPending (cairo_xlib_surface_get_display (sfc)))
      XNextEvent (cairo_xlib_surface_get_display (sfc), &e);
    else
      return;
    
    switch (e.type) {
        case ResizeRequest:
	  *w = ((XResizeRequestEvent)e).width;
	  *h = ((XResizeRequestEvent)e).height;
	  return;
    }
  }
}


cairo_surface_t *cairo_create_x11_surface0(int x, int y) {
  Display *dsp;
  Drawable da;
  int screen;
  cairo_surface_t *sfc;

  if ((dsp = XOpenDisplay(NULL)) == NULL)
    exit(1);

  screen = DefaultScreen(dsp);
  da = XCreateSimpleWindow(dsp, DefaultRootWindow(dsp),
			   0, 0, x, y, 0, 0, 0);
  XSelectInput(dsp, da, ButtonPressMask | KeyPressMask);
  XMapWindow (dsp, da);

  sfc = cairo_xlib_surface_create(dsp, da,
				  DefaultVisual(dsp, screen), x, y);
  cairo_xlib_surface_set_size(sfc, x, y);
  return sfc;
}

void cairo_close_x11_surface(cairo_surface_t *sfc) {
  Display *dsp = cairo_xlib_surface_get_display (sfc);

  cairo_surface_destroy (sfc);
  XCloseDisplay (dsp);
}

int main (int argc, char *argv[]) {
  cairo_surface_t *surface;
  cairo_t *cr;
  int x, y;
  x = y = 500;
  struct timespec ts = {0, 5000000};
  int running;

  surface = cairo_create_x11_surface0(500, 500);
  cr = cairo_create(surface);


  cairo_surface_flush(surface);

  // Await until some key is pressed
  for (running = 1; running;) {

    cairo_set_source_rgb(cr, 0.5, 0.0, 0.5);
    cairo_paint(cr);
    cairo_rectangle(cr, 10, 10, x - 10, y - 10);

  switch (cairo_check_event(surface, 0)) {
    case 0xff1b: // Esc
  case - 1: // Left mouse button
      running = 0;
      break;
  } 

    nanosleep(&ts, NULL);
  }

  cairo_destroy (cr);
  cairo_close_x11_surface(surface);
  return 0;
}
