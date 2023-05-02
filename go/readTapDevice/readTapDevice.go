package main
import (
	"golang.org/x/sys/unix"
	"fmt"
	"os"
	"time"
)

const DEVICENAME = "tap1"

func main() {
	fd, err := makeTap(DEVICENAME)
	defer unix.Close(fd)
	if err != nil {
		fmt.Println("makeTap: ", err.Error())
		os.Exit(1)
	}

	if err := linkUp(DEVICENAME); err != nil {
		fmt.Println("linkUp: ", err.Error())
		os.Exit(1)
	}

	listening := make(chan int, 1)
	go func() {
		received := make([]byte, 10000)
		if n, err := unix.Read(fd, received); err != nil {
			fmt.Println("unix.Read: ", err.Error())
			listening <- 1
		} else {
			received = received[:n]
		}
		fmt.Println(received)

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

func makeTap(dev string) (int, error) {
	// https://mirrors.edge.kernel.org/pub/linux/kernel/people/marcelo/linux-2.4/Documentation/networking/tuntap.txt
	fd, err := unix.Open("/dev/net/tun", unix.O_RDWR, 0)
	if err != nil {
		return -1, fmt.Errorf("unix.Open: %w", err)
	}

	// Setup TAP device
	ifr, err := unix.NewIfreq(dev)
	if err != nil {
		return -1, fmt.Errorf("unix.NewIfreq: %w", err)
	}
	ifr.SetUint16(unix.IFF_TAP)
	if err := unix.IoctlIfreq(fd, unix.TUNSETIFF, ifr); err != nil {
		return -1, fmt.Errorf("unix.IoctlIfreq: %w", err)
	}

	return fd, nil
}

// Equivalent to `ip link set <dev> up'
// According to do_chflags in iplink.c
// https://git.kernel.org/pub/scm/network/iproute2/iproute2.git/tree/ip/iplink.c#n1203
func linkUp(dev string) error {

	// We have to create socket, then do `ioctl' against it
	// TODO: Original code is in get_ctl_fd in iplink.c. Understand how it works.
	fdsock, err := unix.Socket(unix.AF_INET, unix.SOCK_DGRAM, 0)
	if err != nil {
		return fmt.Errorf("unix.Socket: %w", err)
	}

	// Retrive current active flag words.
	ifr, err := unix.NewIfreq(dev)
	if err != nil {
		return fmt.Errorf("unix.NewIfreq for SIOCGIFFLAGS: %w", err)
	}
	if err := unix.IoctlIfreq(fdsock, unix.SIOCGIFFLAGS, ifr); err != nil {
		return fmt.Errorf("unix.IoctlIfreq for SIOCGIFFLAGS: %w", err)
	}

	// Set IFF_UP leaving other stuff unchanged
	ifr.SetUint16((ifr.Uint16() &^ unix.IFF_UP) | unix.IFF_UP)
	if err := unix.IoctlIfreq(fdsock, unix.SIOCSIFFLAGS, ifr); err != nil {
		return fmt.Errorf("unix.IoctlIfreq for SIOCSIFFLAGS: %w", err)
	}
	return nil
}
