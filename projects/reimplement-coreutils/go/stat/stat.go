package main

import (
	"os"
	"fmt"
	"golang.org/x/sys/unix"
	"flag"
	"os/user"
	"time"
)

func main() {
	flag.Parse()
	fileName := flag.Arg(0)
	if fileName == "" {
		fmt.Println("stat: missing operand\nTry 'stat --help' for more information")
		os.Exit(1)
	}

	var st unix.Stat_t
	if err := unix.Lstat(fileName, &st); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	fmt.Println(pp_Stat_t(fileName, st))
}

func showHelp() {
	fmt.Println("Usage: go run stat.go -- [OPTION]... FILE...\n" +
		"Display file or file system status.\n" +
		"\n" +
		"GNU coreutils online help: <https://www.gnu.org/software/coreutils/>")
}

func pp_Stat_t(fn string, st unix.Stat_t) string {
	fileName := fn
	if ((st.Mode & unix.S_IFMT) == unix.S_IFLNK) {
		fileName += " -> "
		linkName := make([]byte, 1000)
		if n, err := unix.Readlink(fn, linkName); err != nil {
			fileName += "FAILED_TO_RETRIVE_NAME(" + fmt.Sprint(n) + ", " + err.Error() + ")"
		} else {
			fileName += string(linkName[:n])
		}
	}

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

	var userName string
	if user, err := user.LookupId(fmt.Sprint(st.Uid)); err == nil {
		userName = user.Username
	} else {
		userName = "FAILED_TO_RETRIVE"
	}

	var groupName string
	if group, err := user.LookupGroupId(fmt.Sprint(st.Gid)); err == nil {
		groupName = group.Name
	} else {
		groupName = "FAILED_TO_RETRIVE"
	}

	var atim time.Time
	{
		sec, nsec := st.Atim.Unix()
		atim = time.Unix(sec, nsec)
	}

	var mtim time.Time
	{
		sec, nsec := st.Mtim.Unix()
		mtim = time.Unix(sec, nsec)
	}

	var ctim time.Time
	{
		sec, nsec := st.Ctim.Unix()
		ctim = time.Unix(sec, nsec)
	}

	var timeFormat = "2006-01-02 15:04:05.000000000 -0700"

	return fmt.Sprintf("  File: %s\n"+
		"  Size: %d\tBlocks: %d\tIO Block: %d\t%s\n"+
		"Device: %d,%d\tInode: %d\tLinks: %d\n"+
		"Access: (%s/%s)\tUid: (%d/ %s)\tGid: (%d/ %s)\n"+
		"Access: %s\n"+
		"Modify: %s\n"+
		"Change: %s",
		// "Birth: %s", // TODO: How can I retrive 'birth'?
		fileName,
		st.Size, st.Blocks, st.Blksize, fileType,
		unix.Major(st.Dev), unix.Minor(st.Dev), st.Ino, st.Nlink,
		permissionNumber(st), permissionLetter(st), st.Uid, userName, st.Gid, groupName,
		atim.Format(timeFormat),
		mtim.Format(timeFormat),
		ctim.Format(timeFormat))
}

func permissionNumber(st unix.Stat_t) string {
	// TODO: what should I do with start "0"?
	return fmt.Sprintf("0%d%d%d", (st.Mode & unix.S_IRWXU)>>6, (st.Mode & unix.S_IRWXG)>>3, st.Mode & unix.S_IRWXO)
}

func permissionLetter(st unix.Stat_t) string {
	result := "" // TODO: What should I do with this? I don't know what it means
	switch st.Mode & unix.S_IFMT {
	case unix.S_IFSOCK: result += "s"
	case unix.S_IFLNK:  result += "l"
	case unix.S_IFREG:  result += "-"
	case unix.S_IFBLK:  result += "b"
	case unix.S_IFDIR:  result += "d"
	case unix.S_IFCHR:  result += "c"
	case unix.S_IFIFO:  result += "p"
	}
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
