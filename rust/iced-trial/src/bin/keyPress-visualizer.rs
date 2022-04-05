use std::collections::HashSet;
use iced_native::keyboard::KeyCode;
use iced_native::subscription::{self, Subscription};
use iced::{Text, Element, Row, Column, Application, Command, Settings, Clipboard, Container,
	   Background, Color};
use iced::{executor, widget::container};
use std::fmt::Debug;

pub struct KeyboardStatus {
    pub pushed_keys: HashSet<KeyCode>,
}

#[derive(Debug, Clone, Copy)]
pub enum Message {
    KeyPressed(KeyCode),
    KeyReleased(KeyCode),
}

struct ContainerStyle {
    is_pressed: bool,
}

impl container::StyleSheet for ContainerStyle {
    fn style(&self) -> container::Style {
	let bg = if self.is_pressed {
	    Color::from_rgb(0.0, 0.0, 0.0)
	} else {
	    Color::from_rgb(1.0, 1.0, 1.0)
	};

	let text = if self.is_pressed {
	    Color::from_rgb(1.0, 1.0, 1.0)
	} else {
	    Color::from_rgb(0.0, 0.0, 0.0)
	};

	container::Style {
	    background: Some(Background::Color(bg)),
	    text_color: Some(text),
	    ..container::Style::default()
	}
    }
}

impl Application for KeyboardStatus {
    type Executor = executor::Default;
    type Message = Message;
    type Flags = ();

    fn title(&self) -> String {
	String::from("Testing keyboard vis")
    }

    fn new(_flags: Self::Flags) -> (Self, Command<Self::Message>) {
	(KeyboardStatus {pushed_keys: HashSet::new()}, Command::none())
    }

    fn update(&mut self, message: Message, _clipboard: &mut Clipboard) -> Command<Message> {
	match message {
	    Message::KeyPressed(keycode) => {
		self.pushed_keys.insert(keycode);
	    }
	    Message::KeyReleased(keycode) => {
		self.pushed_keys.remove(&keycode);
	    }
	};

	Command::none()
    }

    fn view(&mut self) -> Element<'_, Self::Message> {
	let create_row = |key_codes: &[KeyCode]| {
	    key_codes.iter().fold(Row::new(), |row: Row<'_, Message>, key_code| {
		if self.pushed_keys.contains(&key_code) {
		    row.push(Container::new(Text::new(to_key_face(key_code)))
			     .style(ContainerStyle { is_pressed: true }))
		} else {
		    row.push(Container::new(Text::new(to_key_face(key_code)))
			     .style(ContainerStyle { is_pressed: false }))
		}
	    })};

	let first_row = 
	    create_row(&[KeyCode::Key1, KeyCode::Key2, KeyCode::Key3,
			 KeyCode::Key4, KeyCode::Key5, KeyCode::Key6,
			 KeyCode::Key7, KeyCode::Key8, KeyCode::Key9,
			 KeyCode::Key0, KeyCode::Minus, KeyCode::Equals]);
	let second_row =
	    create_row(&[KeyCode::Q, KeyCode::W, KeyCode::E, KeyCode::R,
			KeyCode::T, KeyCode::Y, KeyCode::U, KeyCode::I,
			KeyCode::O, KeyCode::P, KeyCode::LBracket, KeyCode::RBracket]);

	let third_row =
	    create_row(&[KeyCode::A, KeyCode::S, KeyCode::D, KeyCode::F,
			KeyCode::G, KeyCode::H, KeyCode::J, KeyCode::K,
			KeyCode::L, KeyCode::Semicolon, KeyCode::Apostrophe,
			KeyCode::Backslash]);

	let fourth_row =
	    create_row(&[KeyCode::Z, KeyCode::X, KeyCode::C, KeyCode::V,
			 KeyCode::B, KeyCode::N, KeyCode::M, KeyCode::Comma,
			 KeyCode::Period, KeyCode::Slash]);

	Column::new()
	    .push(first_row)
	    .push(Row::new().push(Text::new(" ")).push(second_row))
	    .push(Row::new().push(Text::new("  ")).push(third_row))
	    .push(Row::new().push(Text::new("   ")).push(fourth_row))
	    .into()
    }

    fn subscription(&self) -> Subscription<Self::Message> {
	use iced_native::event::Event;
	use iced_native::keyboard;
	
	subscription::events_with(|event, _status| {
	    match event {
		Event::Keyboard(e) => {
		    match e {
			keyboard::Event::KeyPressed{key_code, modifiers: _} => {
			    Some(Message::KeyPressed(key_code))
			},
			keyboard::Event::KeyReleased{key_code, modifiers: _} => {
			    Some(Message::KeyReleased(key_code))
			}
			_ => None
		    }
		},
		_ => None,
	    }
	})
    }
}

fn to_key_face(key_code: &KeyCode) -> &str {
    match key_code {
	KeyCode::Key1 => "1",
	KeyCode::Key2 => "2",
	KeyCode::Key3 => "3",
	KeyCode::Key4 => "4",
	KeyCode::Key5 => "5",
	KeyCode::Key6 => "6",
	KeyCode::Key7 => "7",
	KeyCode::Key8 => "8",
	KeyCode::Key9 => "9",
	KeyCode::Key0 => "0",

	KeyCode::A => "A",
	KeyCode::B => "B",
	KeyCode::C => "C",
	KeyCode::D => "D",
	KeyCode::E => "E",
	KeyCode::F => "F",
	KeyCode::G => "G",
	KeyCode::H => "H",
	KeyCode::I => "I",
	KeyCode::J => "J",
	KeyCode::K => "K",
	KeyCode::L => "L",
	KeyCode::M => "M",
	KeyCode::N => "N",
	KeyCode::O => "O",
	KeyCode::P => "P",
	KeyCode::Q => "Q",
	KeyCode::R => "R",
	KeyCode::S => "S",
	KeyCode::T => "T",
	KeyCode::U => "U",
	KeyCode::V => "V",
	KeyCode::W => "W",
	KeyCode::X => "X",
	KeyCode::Y => "Y",
	KeyCode::Z => "Z",

	KeyCode::Apostrophe => "'",
	KeyCode::Backslash => "\\",
	KeyCode::Colon => ":",
	KeyCode::Comma => ",",
	KeyCode::Equals => "=",
	KeyCode::LBracket => "[",
	KeyCode::Minus => "-",
	KeyCode::Period => ".",
	KeyCode::Plus => "+",
	KeyCode::RBracket => "]",
	KeyCode::Semicolon => ";",
	KeyCode::Slash => "/",
	_ => "?",
    }
}

fn main() {
    KeyboardStatus::run(Settings::default());
}
