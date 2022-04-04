use iced::button;
use iced::{Button,Column,Text,Settings,Element};
use iced::Sandbox;

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


impl Sandbox for Counter {
    type Message = Message;

    fn new () -> Self {
	Counter{
	    value: 0,
	    increment_button: button::State::new(),
	    decrement_button: button::State::new()
	}
    }

    fn update(&mut self, message: Message) {
	match message {
	    Message::IncrementPressed => {
		self.value += 1;
	    }
	    Message::DecrementPressed => {
		self.value -= 1;
	    }
	}
    }

    fn title(&self) -> String {
	String::from("test app")
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


}

fn main() -> iced::Result {
    Counter::run(Settings::default())
}
