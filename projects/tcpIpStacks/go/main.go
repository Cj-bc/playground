package main
import (
	"golang.org/x/sys/unix"
	"fmt"
	"os"
	"time"
	"github.com/Cj-bc/plg/tcpIpStacks/tap"
	"github.com/Cj-bc/plg/tcpIpStacks/ether"
)

const DEVICENAME = "tap1"

func main() {
	tap, err := tap.New(DEVICENAME)
	defer tap.Close()
	if err != nil {
		fmt.Println("makeTap: ", err.Error())
		os.Exit(1)
	}

	if err := tap.Up(); err != nil {
		fmt.Println("linkUp: ", err.Error())
		os.Exit(1)
	}

	// Try to receiving frames
	listening := make(chan int, 1)
	go func() {
		received := make([]byte, 10000)
		if n, err := unix.Read(tap.Fd, received); err != nil {
			fmt.Println("unix.Read: ", err.Error())
			listening <- 1
		} else {
			received = received[:n]
		}

		if header, err := ether.GetHeader(received); err != nil {
			fmt.Println("parseEther: ", err.Error())
			os.Exit(1)
		} else {
			fmt.Printf("from: %x to: %x type: %x\n", header.AddrFrom, header.AddrTo, header.InnerType)
		}
		listening <- 0
	}()

	timeout := time.After(20 * time.Second)

OuterLoop:
	for {
		select {
		case <-timeout:
			break OuterLoop
		case <-listening:
			break OuterLoop
		}
	}
}
