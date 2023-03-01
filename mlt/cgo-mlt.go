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

// You should close both playlist and profile at caller side
func makePlaylist(files []string) (pl C.mlt_playlist, profile C.mlt_profile) {
	profile = C.mlt_profile_init(nil)
	pl = makePlaylistWithProfile(profile, files)

	pl_producer := C.mlt_playlist_producer(pl)
	defer C.mlt_producer_close(pl_producer)

	C.mlt_profile_from_producer(profile, pl_producer)

	pl = makePlaylistWithProfile(profile, files)

	return
}

func makePlaylistWithProfile(profile C.mlt_profile, files []string) (playlist C.mlt_playlist) {
	playlist = C.mlt_playlist_init()
	for _, f := range files {
		fn := unsafe.Pointer(C.CString(f))
		defer C.free(fn)
		prod := C.mlt_factory_producer(profile, nil, fn)
		C.mlt_playlist_append(playlist, prod)
		C.mlt_producer_close(prod)
	}

	return
}

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

	playlist, _ := makePlaylist(files)

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
