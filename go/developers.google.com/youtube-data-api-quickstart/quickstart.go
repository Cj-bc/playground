package main

import (
  "fmt"
  "log"
  "os"
  "golang.org/x/net/context"
  "google.golang.org/api/youtube/v3"
  "google.golang.org/api/option"
)


func handleError(err error, message string) {
	if message == "" {
		message = "Error making API call"
	}
	if err != nil {
		log.Fatalf(message + ": %v", err.Error())
	}
}

func channelsListByUsername(service *youtube.Service, part string, forUsername string) {
	call := service.Channels.List([]string{part})
	call = call.ForUsername(forUsername)
	response, err := call.Do()
	handleError(err, "")
	fmt.Println(fmt.Sprintf("This channel's ID is %s. Its title is '%s', " +
		"and it has %d viewers.",
		response.Items[0].Id,
		response.Items[0].Snippet.Title,
		response.Items[0].Statistics.ViewCount))

}

func main() {
	ctx := context.Background()
	key, err := os.ReadFile("key.txt")
	if err != nil {
		log.Fatalf("Could not read token file: %v", err)
	}

	service, err := youtube.NewService(ctx,
		option.WithAPIKey(string(key)))

	handleError(err, "Error creating YouTube Client")
	channelsListByUsername(service, "snippet,contentDetails,statistics", "GoogleDevelopers")
}
