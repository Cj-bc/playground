package main

/*
#cgo pkg-config: mlt-framework-7
#include <framework/mlt.h>
#include <stdlib.h>
*/
import "C"
import ("fmt"
	"flag"
	"unsafe"
)

func main() {

	flag.Parse()
	files := flag.Args()

	if len(files) == 0 {
		usage()
		return
	}


	fmt.Println("Initializing factory...")
	repo := C.mlt_factory_init(nil)
	defer C.mlt_factory_close()

	if repo == nil {
		fmt.Println("Failed to initialize factory")
		return
	}

	profile := C.mlt_profile_init(nil)

	playlist := C.mlt_playlist_init()
	defer C.mlt_playlist_close(playlist)
	for _, v := range files {
		fn := C.CString(v)
		defer C.free(unsafe.Pointer(fn))
		prod := C.mlt_factory_producer(profile, nil, unsafe.Pointer(fn))
		defer C.mlt_producer_close(prod)

		C.mlt_playlist_append(playlist, prod)
	}

	for i := 0; i < int(C.mlt_playlist_count(playlist)); i++ {
		var info C.mlt_playlist_clip_info
		C.mlt_playlist_get_clip_info(playlist, &info, C.int(i))
		fmt.Printf("%v\n", C.GoString(info.resource))
	}

	fmt.Println("End of main. Closing factory...")
}

// Displays help message
func usage() {
	fmt.Println("simple-media-player: plays movies in serial form\nsimple-media-player [MOVIE_PATH ...]")
}
