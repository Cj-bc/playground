package main

import (
	"context"
	"go-server/ent"
	"go-server/ent/todo"
	"log"
	"net/http"
	"strconv"

	"github.com/gin-gonic/gin"

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
		log.Fatalf("failed creating schema resources: %v", err)
	}
	// Preparing client done

	// client.Todo.Create().
	// 	SetTitle("This is test todo").
	// 	SetIsDone(false).Save(context.Background())

	router := gin.Default()
	router.GET("/todoes", endpointTodoesHandler(client))
	router.GET("/todo/:todo_id", todoController(client))
	router.Run(":3000")
}

func endpointTodoesHandler(client *ent.Client) gin.HandlerFunc {
	return func(c *gin.Context) {
		result, err := client.Todo.Query().All(c)
		if err != nil {
			log.Fatalf("failed querying")
		}
		log.Println("found toodes: %v", result)

		var msg struct {
			todoes []*ent.Todo `json:"todoes"`
			title  string      `json:"title"`
		}
		msg.todoes = result
		msg.title = "FooBar"

		c.JSON(http.StatusOK, struct{ Todoes []*ent.Todo }{Todoes: result})
	}
}

func todoController(client *ent.Client) gin.HandlerFunc {
	return func(c *gin.Context) {
		id_str := c.Param("todo_id")
		id, err := strconv.Atoi(id_str)
		if err != nil {
			c.JSON(http.StatusBadRequest, struct{}{})
			return
		}

		entry, err := client.Todo.Query().Where(todo.ID(id)).Only(c)
		if err != nil {
			c.JSON(http.StatusNotFound, struct{}{})
			return
		}

		c.JSON(http.StatusOK, entry)
	}
}
