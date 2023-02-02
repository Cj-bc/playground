package main

import (
	"context"
	"go-server/awsResources"
	"go-server/ent"
	"go-server/ent/todo"
	"log"
	"net/http"
	"strconv"

	"github.com/gin-contrib/cors"
	"github.com/gin-gonic/gin"

	_ "github.com/mattn/go-sqlite3"
)

var FRONT_PORT int = 80
func main() {

	// Preparing AWS stuff
	ctx := context.Background()

	selfIp, err := awsResources.SelfIpAddress(ctx)
	if err != nil {
		log.Fatalf("failed retriving frontend server ip address: %v", err)
	}
	frontOrigin := "http://" + selfIp + ":" + strconv.Itoa(FRONT_PORT)

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

	router := gin.Default()
	router.Use(cors.New(cors.Config{
		AllowOrigins: []string{frontOrigin},
		AllowMethods: []string{"PATCH", "POST", "GET", "DELETE"},
		AllowHeaders: []string{"Content-Type"},
	}))
	router.GET("/todoes", endpointTodoesHandler(client))
	router.POST("/todo", newTodoController(client))
	router.GET("/todo/:todo_id", todoController(client))
	router.PATCH("/todo/:todo_id", updateTodoController(client))
	router.DELETE("/todo/:todo_id", removeTodoController(client))
	router.Run(":80")
}

func endpointTodoesHandler(client *ent.Client) gin.HandlerFunc {
	return func(c *gin.Context) {
		result, err := client.Todo.Query().All(c)
		if err != nil {
			log.Fatalf("failed querying")
		}

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

func updateTodoController(client *ent.Client) gin.HandlerFunc {
	return func(c *gin.Context) {
		id_str := c.Param("todo_id")
		id, err := strconv.Atoi(id_str)
		if err != nil {
			c.JSON(http.StatusBadRequest, struct{}{})
			return
		}

		var body struct {
			Title  *string `json:"title"`
			IsDone *bool   `json:"isDone"`
		}
		if err := c.ShouldBindJSON(&body); err != nil {
			c.JSON(http.StatusBadRequest, struct{}{})
			return
		}

		updater := client.Todo.UpdateOneID(id)
		if body.IsDone != nil {
			updater.SetIsDone(*body.IsDone)
		}
		if body.Title != nil {
			updater.SetTitle(*body.Title)
		}

		if err := updater.Exec(c); err != nil {
			c.JSON(http.StatusInternalServerError, struct{}{})
			return
		}
		c.JSON(http.StatusOK, struct{}{})
	}
}

func newTodoController(client *ent.Client) gin.HandlerFunc {
	return func(c *gin.Context) {
		var newTodo struct {
			Title string `json:"title"`
		}
		if err := c.ShouldBindJSON(&newTodo); err != nil {
			c.JSON(http.StatusBadRequest, struct{}{})
			return
		}

		todo, err := client.Todo.Create().
			SetTitle(newTodo.Title).
			SetIsDone(false).
			Save(c)
		if err != nil {
			c.JSON(http.StatusInternalServerError, struct{}{})
			return
		}

		c.JSON(http.StatusOK, todo)
	}
}

func removeTodoController(client *ent.Client) gin.HandlerFunc {
	return func(c *gin.Context) {
		id_str := c.Param("todo_id")
		id, err := strconv.Atoi(id_str)
		if err != nil {
			c.JSON(http.StatusBadRequest, struct{}{})
			return
		}

		if err := client.Todo.DeleteOneID(id).Exec(c); err != nil {
			c.Status(http.StatusInternalServerError)
			return
		}
		c.Status(http.StatusOK)
	}
}
