package main

import (
    "net/http"
    "os"
    "fmt"
    "log"
    "flag"
)

type binaryFiles struct {
	path []string
}

func (bf binaryFiles) String() string {
	return fmt.Sprint(bf.path)
}

func (bf *binaryFiles) Set(s string) error {
	if bf == nil {
		bf.path = []string{}
	}
	bf.path = append(bf.path, s)
	return nil
}

func GetFileContent(path string) ([]byte, error) {
	f, err := os.Open(path)
	defer f.Close()
	if err != nil {
		return []byte{}, err
	}
	
	stat, err := f.Stat()
	if err != nil {
		return []byte{}, err
	}

	buf := make([]byte, stat.Size())
	if _, err = f.Read(buf); err != nil {
		return []byte{}, err
	}

    return buf, nil
}

func main() {
	var binaries binaryFiles
	flag.Var(&binaries, "binary", "files that shuold be sent as binary")
	flag.Parse()
	files := flag.Args()
	if flag.NArg() == 0 {
		log.Fatal("At least one file should be specified to serve.")
	}
	
	for _, fn := range files {
		content, err := GetFileContent(fn)
		if err != nil {
			log.Fatalf("Failed to read file \"%s\" (error: %v)", fn, err)
		}

		http.HandleFunc(fmt.Sprintf("/%s", fn), func(w http.ResponseWriter, r *http.Request) {
			fmt.Fprint(w, string(content))
		})
	}

	for _, fn := range binaries.path {
		content, err := GetFileContent(fn)
		if err != nil {
			log.Fatalf("Failed to read file \"%b\" (error: %v)", fn, err)
		}

		http.HandleFunc(fmt.Sprintf("/%s", fn), func(w http.ResponseWriter, r*http.Request) {
			w.Header().Set("Content-Type", "application/octed-stream")
			w.Write(content)
		})
	}
	
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprint(w, "<html> <body>")
		defer fmt.Fprint(w, "</body></html>")

		fmt.Fprint(w, "<h1>Regular files</h1><ul>")
		for _, f := range files {
			fmt.Fprintf(w, "<li><a href=\"%s\">%s</a></li>", f, f)
		}
		fmt.Fprint(w, "</ul> <h1>Binary Files</h1><ul>")
		for _, f := range binaries.path {
			fmt.Fprintf(w, "<li><a href=\"%s\">%s</a></li>", f, f)
		}
		fmt.Fprint(w, "</ul>")
	})
	http.ListenAndServe(":8080", nil)
}
