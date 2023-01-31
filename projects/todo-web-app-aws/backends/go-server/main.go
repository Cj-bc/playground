package main

import (
	"context"
	"fmt"
	"go-server/ent"
	"go-server/ent/todo"
	"log"

	_ "github.com/mattn/go-sqlite3"
)

func main() {
	// Preparing client
	client, err := ent.Open("sqlite3", "./todoes.sqlite3?_fk=1")
	if err != nil {
		log.Fatalf("failed opening connection to sqlite: %v", err)
	}
	defer client.Close()

	if err := client.Schema.Create(context.Background()); err != nil {
		log.Fatalf ("failed creating schema resources: %v", err)
	}
	// Preparing client done


}
