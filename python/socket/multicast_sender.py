from socket import *

multicast_group = "239.255.0.1"
port = 5002


s = socket(AF_INET, SOCK_DGRAM)
s.setsockopt(SOL_SOCKET, SO_BROADCAST, 1)
s.setsockopt(IPPROTO_IP, IP_MULTICAST_IF, inet_aton("127.0.0.1"))


try:
    c=0
    while True:
        s.sendto(b"hello message: " + str(c).encode(), (multicast_group, port))
        c+=1
except KeyboardInterrupt:
    pass
finally:
    print("closing socket")
    s.close()
    print("socket closed")
