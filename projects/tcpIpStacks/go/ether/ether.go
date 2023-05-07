package ether

import (
	"fmt"
	"bytes"
	"encoding/binary"
)

type EtherHeader struct {
	AddrTo [6]byte
	AddrFrom [6]byte
	InnerType [2]byte
}

func GetHeader(bs []byte) (EtherHeader, error) {
	header := EtherHeader{}
	// Truncate first 4 bytes because they're TUNTAP header. I'll treat them in tap package later
	buf := bytes.NewReader(bs[4:])
	if err := binary.Read(buf, binary.BigEndian, &header); err != nil {
		return header, fmt.Errorf("Failed to parse ethernet header")
	}
	return header, nil
}
