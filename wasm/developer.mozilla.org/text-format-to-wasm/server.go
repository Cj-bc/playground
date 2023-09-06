package main

import (
    "net/http"
    "os"
    "fmt"
    "log"
)

func GetFileContent(path string) (string, error) {
    f, err := os.Open(path)
    defer f.Close()
    if err != nil {
        return "", err
    }

    stat, err := f.Stat()
    if err != nil {
        return "", err
    }

	buf := make([]byte, stat.Size())
    if _, err = f.Read(buf); err != nil {
        return "", err
    }

    return string(buf), nil
}

func main() {
    index, err := GetFileContent("index.html")
    if err != nil {
   		log.Fatalf("failed to read from index.html: %v", err)
    }
    wasm, err := GetFileContent("simple.wasm")
    if err != nil {
   		log.Fatalf("failed to read from simple.wasm: %v", err)
    }

	http.HandleFunc("/index.html", func(w http.ResponseWriter, r *http.Request) {
    	fmt.Fprint(w, index)
	})
	http.HandleFunc("/simple.wasm", func(w http.ResponseWriter, r *http.Request) {
    	fmt.Fprint(w, wasm)
	})

	http.ListenAndServe(":8080", nil)
}
