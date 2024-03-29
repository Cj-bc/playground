use iced_graphics::widget::canvas::{self, Canvas, Cursor, Fill, Frame, Geometry, Path, Program, Stroke};
use iced::{Color, Rectangle, Settings, Application, Element,
	   Executor, executor, Clipboard, Command};

use std::time::Instant;

struct Rings {
    inner_ring_radius: f32,
    outer_ring_radius: f32,
    start: Instant,
    now: Instant,
}

impl Program<Message> for Rings {
    fn draw(&self, bounds: Rectangle, _cursor: Cursor) -> Vec<Geometry> {
	let mut frame = Frame::new(bounds.size());

	let elapsed = self.now - self.start;
	let inner_circle = Path::circle(frame.center(), (elapsed.as_secs() as f32).sin() * self.inner_ring_radius);
	let outer_circle = Path::circle(frame.center(), (elapsed.as_secs() as f32).cos() * self.outer_ring_radius);

	

	frame.stroke(&inner_circle, Stroke::default());
	frame.stroke(&outer_circle, Stroke {
	    color: Color::from_rgb(0.5, 0.5, 0.5),
	    ..Stroke::default()});

	vec![frame.into_geometry()]
    }
}


#[derive(Debug)]
enum Message {
    Tick
}

impl Application for Rings {
    type Message = Message;
    type Executor = executor::Default;
    type Flags = ();

    fn new(_flags: Self::Flags) -> (Self, Command<Message>) {
	(Rings { inner_ring_radius: 5.0,
		 outer_ring_radius: 10.0,
		 start: Instant::now(),
		 now: Instant::now(),
	}, Command::none())
    }

    fn title(&self) -> String {
	String::from("test")
    }

    fn update(&mut self, message: Self::Message,
	      _clipboard: &mut Clipboard) -> Command<Message> {
	match message {
	    Tick => self.now = Instant::now(),
	};

	Command::none()
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
