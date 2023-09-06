package main

import (
    "net/http"
    "os"
    "fmt"
    "log"
    "flag"
)

type binaryFiles []string

func (bf binaryFiles) String() string {
    return fmt.Sprint(bf)
}

func (bf *binaryFiles) Set(s string) error {
    *bf = append(*bf, s)
    return nil
}

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
    // var binaries binaryFiles
    // flag.Var(binaries, "binary", "files that shuold be sent as binary")
    flag.Parse()
    files := flag.Args()
    if len(files) == 0 {
        log.Fatal("At least one file should be specified to serve.")
    }

	for _, fn := range files {
		content, err := GetFileContent(fn)
		if err != nil {
    		log.Fatalf("Failed to read file \"%s\" (error: %v)", fn, err)
		}

		http.HandleFunc(fmt.Sprintf("/%s", fn), func(w http.ResponseWriter, r *http.Request) {
    		fmt.Fprint(w, content)
		})
	}

	http.HandleFunc("/list", func(w http.ResponseWriter, r *http.Request) {
    	fmt.Fprintf(w, "%v", files)
	})
	http.ListenAndServe(":8080", nil)
}
