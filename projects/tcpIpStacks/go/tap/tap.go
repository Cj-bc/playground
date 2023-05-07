package tap
import (
	"golang.org/x/sys/unix"
	"fmt"
)

type Tap struct {
	Fd int
	Name string
}

func New(name string) (Tap, error) {
	// https://mirrors.edge.kernel.org/pub/linux/kernel/people/marcelo/linux-2.4/Documentation/networking/tuntap.txt
	fd, err := unix.Open("/dev/net/tun", unix.O_RDWR, 0)
	if err != nil {
		return Tap{}, fmt.Errorf("failed to Open /dev/net/tun file with O_RDWR: %w", err)
	}

	// Setup TAP device
	ifr, err := unix.NewIfreq(name)
	if err != nil {
		return Tap{}, fmt.Errorf("Failed to initialize ifreq for TUNSETIFF: %w", err)
	}
	ifr.SetUint16(unix.IFF_TAP)
	if err := unix.IoctlIfreq(fd, unix.TUNSETIFF, ifr); err != nil {
		return Tap{}, fmt.Errorf("Failed to call ioctl for TUNSETIFF with IFF_TAP: %w", err)
	}
	return Tap{fd, name}, nil
}


// Equivalent to `ip link set <dev> up'
// According to do_chflags in iplink.c
// https://git.kernel.org/pub/scm/network/iproute2/iproute2.git/tree/ip/iplink.c#n1203
//
// Make sure device `dev' is available before call this
func (tap *Tap) Up() error {

	// We have to create socket, then do `ioctl' against it
	// TODO: Original code is in get_ctl_fd in iplink.c. Understand how it works.
	fdsock, err := unix.Socket(unix.AF_INET, unix.SOCK_DGRAM, 0)
	if err != nil {
		return fmt.Errorf("Failed to create socket with AF_INET, SOCK_DGRAM: %w", err)
	}

	// Retrive current active flag words.
	ifr, err := unix.NewIfreq(tap.Name)
	if err != nil {
		return fmt.Errorf("Failed to create Ifreq for SIOCGIFFLAGS: %w", err)
	}
	if err := unix.IoctlIfreq(fdsock, unix.SIOCGIFFLAGS, ifr); err != nil {
		return fmt.Errorf("Failed to call ioctl for SIOCGIFFLAGS: %w", err)
	}

	// Set IFF_UP leaving other stuff unchanged
	ifr.SetUint16((ifr.Uint16() &^ unix.IFF_UP) | unix.IFF_UP)
	if err := unix.IoctlIfreq(fdsock, unix.SIOCSIFFLAGS, ifr); err != nil {
		return fmt.Errorf("Failed to call ioctl for SIOCSIFFLAGS: %w", err)
	}

	return nil
}

// Close tap device
func (tap *Tap) Close() {
	unix.Close(tap.Fd)
}

// Read from Tap device, and return contained Ethernet frame.
// It'll truncate TUNTAP header
func (tap Tap) Read(p []byte) (n int, err error) {
	// TODO: Can't I remove 'buf' variable?
	// I couldn't write something like 'p = p[4:]' but it didn't work.
	buf := make([]byte, len(p), cap(p))
	n, err = unix.Read(tap.Fd, buf)
	copy(p, buf[4:])
	return
}
