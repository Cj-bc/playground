package greetings

import "fmt"

func Hello(name string) sring {
     // Return a greeting that embeds the name in a message.
     message := fmt.Sprintf("Hi, %v. Welcome!", name)
     return message
}