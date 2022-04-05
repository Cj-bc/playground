use std::collections::HashMap;
use iced_native::keyboard::KeyCode;
use iced_native::subscription::{self, Subscription};
use iced::{Text, Element, Row, Application, Command, Settings, Clipboard};
use iced::executor;
use std::fmt::Debug;

pub struct KeyboardStatus {
    pub keys: HashMap<KeyCode, bool>,
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
	let initial = HashMap::from(
	    [ KeyCode::A, KeyCode::B, KeyCode::C, KeyCode::D
	      , KeyCode::E, KeyCode::F, KeyCode::G, KeyCode::H
	      , KeyCode::I, KeyCode::J, KeyCode::K, KeyCode::L
	      , KeyCode::M, KeyCode::N, KeyCode::O, KeyCode::P
	      , KeyCode::Q, KeyCode::R, KeyCode::S, KeyCode::T
	      , KeyCode::U, KeyCode::V, KeyCode::W, KeyCode::X
	      , KeyCode::Y, KeyCode::Z, KeyCode::Key0, KeyCode::Key1
	      , KeyCode::Key2, KeyCode::Key3, KeyCode::Key4, KeyCode::Key5
	      , KeyCode::Key6, KeyCode::Key7, KeyCode::Key8, KeyCode::Key9
	    ].map(|k| (k, false)));

	(KeyboardStatus {keys: initial}, Command::none())
    }

    fn update(&mut self, message: Message, clipboard: &mut Clipboard) -> Command<Message> {
	match message {
	    Message::KeyPressed(keycode) => {
		self.keys.entry(keycode).and_modify(|e| {*e = true});
	    }
	    Message::KeyReleased(keycode) => {
		self.keys.entry(keycode).and_modify(|e| {*e = false});
	    }
	};

	Command::none()
    }

    fn view(&mut self) -> Element<'_, Self::Message> {
	self.keys.iter().fold(Row::new(), |row, (keyCode, is_pressed)| {
	    if *is_pressed {
		row.push(Text::new(format!("{:?}", keyCode))
			       .color(iced_native::Color::BLACK))
	    } else {
		row.push(Text::new(format!("{:?}", keyCode))
			       .color(iced_native::Color::WHITE))
	    }
	}).into()
    }

    fn subscription(&self) -> Subscription<Self::Message> {
	use iced_native::event::Event;
	use iced_native::keyboard;
	
	subscription::events_with(|event, _status| {
	    match event {
		Event::Keyboard(e) => {
		    match e {
			keyboard::Event::KeyPressed{key_code: keyCode, modifiers: _} => {
			    Some(Message::KeyPressed(keyCode))
			},
			keyboard::Event::KeyReleased{key_code: keyCode, modifiers: _} => {
			    Some(Message::KeyReleased(keyCode))
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
