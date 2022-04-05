use iced::button;
use iced::{Button,Column,Text,Settings,Element};
use iced::Application;
use iced::executor;
use iced::Command;
use iced::Clipboard;
use iced::Subscription;

struct Counter {
    // The counter value
    value: i32,

    // The local state of the two buttons
    increment_button: button::State,
    decrement_button: button::State,
}

#[derive(Debug, Clone, Copy)]
pub enum Message {
    IncrementPressed,
    DecrementPressed,
}


impl Application for Counter {

    type Executor = executor::Default;
    type Message = Message;
    type Flags = ();

    fn new (flags: Self::Flags) -> (Self, Command<Self::Message>) {
	(Counter{
	    value: 0,
	    increment_button: button::State::new(),
	    decrement_button: button::State::new()
	},
	 Command::none())
    }

    fn title(&self) -> String {
	String::from("test app")
    }

    fn update(&mut self, message: Message, clipboard: &mut Clipboard) -> Command<Message> {
	match message {
	    Message::IncrementPressed => {
		self.value += 1;
	    }
	    Message::DecrementPressed => {
		self.value -= 1;
	    }
	};

	Command::none()
    }

    fn view(&mut self) -> Element<'_, Self::Message> {
	Column::new()
	    .push(
		Button::new(&mut self.increment_button, Text::new("+"))
		    .on_press(Message::IncrementPressed),
	    )
	    .push(
		Text::new(self.value.to_string()).size(50),

	    )
	    .push(
		Button::new(&mut self.decrement_button, Text::new("-"))
		    .on_press(Message::DecrementPressed),
	    ).into()
    }

    fn subscription(&self) -> Subscription<Self::Message> {
	use iced_native::keyboard;
	use iced_native::Event;
	
	iced_native::subscription::events_with(|event, status|
		match event {
		    Event::Keyboard(keyboard::Event::KeyPressed {
			key_code,
			modifiers: _,}) => match key_code {
			keyboard::KeyCode::J => Some(Message::DecrementPressed),
			keyboard::KeyCode::K => Some(Message::IncrementPressed),
			_ => None,
		    }

		    _ => None
	    })
    }

}

fn main() -> iced::Result {
    Counter::run(Settings::default())
}
