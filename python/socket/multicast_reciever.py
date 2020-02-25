from socket import *

s = socket(AF_INET, SOCK_DGRAM)
port = 5002

s.setsockopt(SOL_SOCKET, SO_REUSEADDR, 1) # enable reusing the same address on the same time
s.bind(('', port))
try:
    while True:
        msg = s.recv(48)
        print(msg)
except KeyboardInterrupt:
    pass
finally:
    s.close()
