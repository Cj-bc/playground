use iced_graphics::widget::canvas::{self, Canvas, Cursor, Fill, Frame, Geometry, Path, Program, Stroke};
use iced::{Color, Rectangle, Settings, Sandbox, Element};

struct Rings {
    inner_ring_radius: f32,
    outer_ring_radius: f32,
}

impl Program<Message> for Rings {
    fn draw(&self, bounds: Rectangle, _cursor: Cursor) -> Vec<Geometry> {
	let mut frame = Frame::new(bounds.size());

	let inner_circle = Path::circle(frame.center(), self.inner_ring_radius);
	let outer_circle = Path::circle(frame.center(), self.outer_ring_radius);

	frame.stroke(&inner_circle, Stroke::default());
	frame.stroke(&outer_circle, Stroke {
	    color: Color::from_rgb(0.5, 0.5, 0.5),
	    ..Stroke::default()});

	vec![frame.into_geometry()]
    }
}


#[derive(Debug)]
enum Message {
    NoMessage
}

impl Sandbox for Rings {
    type Message = Message;

    fn new() -> Self {
	Rings { inner_ring_radius: 5.0,
		outer_ring_radius: 10.0}
    }

    fn title(&self) -> String {
	String::from("test")
    }

    fn update(&mut self, message: Self::Message) {

    }

    fn view(&mut self) -> Element<'_, Self::Message> {
	Canvas::new(self).into()
    }
}
fn main() {
    Rings::run(Settings {
	antialiasing: true,
	..Settings::default()});
}
