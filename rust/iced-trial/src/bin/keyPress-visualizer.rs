use std::collections::HashSet;
use iced_native::keyboard::KeyCode;
use iced_native::subscription::{self, Subscription};
use iced::{Text, Element, Row, Column, Application, Command, Settings, Clipboard};
use iced::executor;
use std::fmt::Debug;

pub struct KeyboardStatus {
    pub pushed_keys: HashSet<KeyCode>,
}

#[derive(Debug, Clone, Copy)]
pub enum Message {
    KeyPressed(KeyCode),
    KeyReleased(KeyCode),
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
	let create_row = |to_text: &dyn Fn(KeyCode) -> String, key_codes: &[KeyCode]| {
	    key_codes.iter().fold(Row::new(), |row: Row<'_, Message>, key_code| {
		if self.pushed_keys.contains(&key_code) {
		    row.push(Text::new(to_text(*key_code)).color(iced_native::Color::BLACK))
		} else {
		    row.push(Text::new(to_text(*key_code))
			     .color(iced_native::Color::from_rgb(0.8, 0.8, 0.8)))
		}
	    })};

	let first_row = 
	    create_row(&|k| format!("{:?}", k).strip_prefix("Key").unwrap().to_string(),
		       &[KeyCode::Key1, KeyCode::Key2, KeyCode::Key3,
			 KeyCode::Key4, KeyCode::Key5, KeyCode::Key6,
			 KeyCode::Key7, KeyCode::Key8, KeyCode::Key9,
			 KeyCode::Key0]);
	let second_row =
	    create_row(&|k| format!("{:?}", k),
	               &[KeyCode::Q, KeyCode::W, KeyCode::E, KeyCode::R,
			KeyCode::T, KeyCode::Y, KeyCode::U, KeyCode::I,
			KeyCode::O, KeyCode::P]);

	let third_row =
	    create_row(&|k| format!("{:?}", k),
	               &[KeyCode::A, KeyCode::S, KeyCode::D, KeyCode::F,
			KeyCode::G, KeyCode::H, KeyCode::J, KeyCode::K,
			KeyCode::L]);

	let fourth_row =
	    create_row(&|k| format!("{:?}", k),
	               &[KeyCode::Z, KeyCode::X, KeyCode::C, KeyCode::V,
			 KeyCode::B, KeyCode::N, KeyCode::M, KeyCode::Comma,
			 KeyCode::Period]);


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

fn main() {
    KeyboardStatus::run(Settings::default());
}
