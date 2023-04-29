package main
import "flag"
import "os"
import "golang.org/x/sys/unix"
import "fmt"


func to(stat unix.Stat_t) string {
    fileType := ""
    switch stat.Mode & unix.S_IFMT {
        case unix.S_IFSOCK: fileType = "socket"
        case unix.S_IFLNK: fileType = "Symlink"
        case unix.S_IFREG: fileType = "RegularFile"
        case unix.S_IFBLK: fileType = "BlockDevice"
        case unix.S_IFDIR: fileType = "Directory"
        case unix.S_IFCHR: fileType = "CharacterDevice"
        case unix.S_IFIFO: fileType = "FIFO"
        default: fileType = "Unknown"
    }
    return fmt.Sprintf("dev: %s\ninode: %d\n", fileType, stat.Ino)
}

func main() {
  flag.Parse()
  fn := flag.Arg(0)

  if (fn == "") {
  	fmt.Println("No file is given")
	os.Exit(-1)
  }

  var stat unix.Stat_t
  if err := unix.Stat(fn, &stat); err != nil {
	fmt.Print(err.Error())
	os.Exit(-1)
  }

  fmt.Print(to(stat))
}
