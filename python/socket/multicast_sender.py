from socket import *

s = socket(AF_INET, SOCK_DGRAM)
s.setsockopt(SOL_SOCKET, SO_BROADCAST, 1)
s.setsockopt(IPPROTO_IP, IP_MULTICAST_LOOP, 0)
s.setsockopt(IPPROTO_IP, IP_MULTICAST_IF, inet_aton("127.0.0.1"))


try:
    while True:
        s.sendto(b"abcdef", ("localhost", 5003))
except KeyboardInterrupt:
    pass
finally:
    print("closing socket")
    s.close()
    print("socket closed")
