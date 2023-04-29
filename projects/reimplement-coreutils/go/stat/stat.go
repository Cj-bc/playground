package main

import (
	"os"
	"fmt"
	"golang.org/x/sys/unix"
	"flag"
)

func main() {
	flag.Parse()
	fileName := flag.Arg(0)
	if fileName == "" {
		fmt.Println("stat: missing operand\nTry 'stat --help' for more information")
		os.Exit(1)
	}

	var st unix.Stat_t
	unix.Stat(fileName, &st)

	fmt.Println(pp_Stat_t(fileName, st))
}

func showHelp() {
	fmt.Println("Usage: go run stat.go -- [OPTION]... FILE...\n" +
		"Display file or file system status.\n" +
		"\n" +
		"GNU coreutils online help: <https://www.gnu.org/software/coreutils/>")
}

func pp_Stat_t(fn string, st unix.Stat_t) string {
	var fileType string
	switch st.Mode & unix.S_IFMT {
	case unix.S_IFSOCK: fileType = "socket"
	case unix.S_IFLNK:  fileType = "symbolic link"
	case unix.S_IFREG:  fileType = "regular file"
	case unix.S_IFBLK:  fileType = "block special device"
	case unix.S_IFDIR:  fileType = "directory"
	case unix.S_IFCHR:  fileType = "character special device"
	case unix.S_IFIFO:  fileType = "fifo"
	}

	return fmt.Sprintf("File: %s\nSize: %d\tBlocks: %d\t%s\nDevice: %d,%d\tInode: %d\tLinks: %d\nAccess: (%s/%s)",
		fn,
		st.Size, st.Blksize, fileType,
		unix.Major(st.Dev), unix.Minor(st.Dev), st.Ino, st.Nlink,
		permissionNumber(st), permissionLetter(st))
}

func permissionNumber(st unix.Stat_t) string {
	return fmt.Sprint("0", (st.Mode & unix.S_IRWXU)>>6, (st.Mode & unix.S_IRWXG)>>3, st.Mode & unix.S_IRWXO)
}

func permissionLetter(st unix.Stat_t) string {
	result := "-"
	if (st.Mode & unix.S_IRUSR != 0) { result += "r"; } else { result += "-"; }
	if (st.Mode & unix.S_IWUSR != 0) { result += "w"; } else { result += "-"; }
	if (st.Mode & unix.S_IXUSR != 0) { result += "x"; } else { result += "-"; }
	if (st.Mode & unix.S_IRGRP != 0) { result += "r"; } else { result += "-"; }
	if (st.Mode & unix.S_IWGRP != 0) { result += "w"; } else { result += "-"; }
	if (st.Mode & unix.S_IXGRP != 0) { result += "x"; } else { result += "-"; }
	if (st.Mode & unix.S_IROTH != 0) { result += "r"; } else { result += "-"; }
	if (st.Mode & unix.S_IWOTH != 0) { result += "w"; } else { result += "-"; }
	if (st.Mode & unix.S_IXOTH != 0) { result += "x"; } else { result += "-"; }

	return result
}
