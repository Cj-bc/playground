from socket import *

s = socket(AF_INET, SOCK_DGRAM)
multicast_group = "239.255.0.1"
multicast_if_addr = "0.0.0.1"
port = 5002

s.setsockopt(SOL_SOCKET, SO_REUSEADDR, 1) # enable reusing the same address on the same time
s.bind(('', port))

# Create 'ip_mreq' structure (in c)
mreq = inet_aton(multicast_group)+inet_aton(multicast_if_addr)

s.setsockopt(IPPROTO_IP, IP_ADD_MEMBERSHIP, mreq) # Join multicast group
try:
    while True:
        msg = s.recv(48)
        print(msg)
except KeyboardInterrupt:
    pass
finally:
    s.close()
