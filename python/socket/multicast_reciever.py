from socket import *

s = socket(AF_INET, SOCK_DGRAM)
s.bind(("localhost", 5003))

try:
    while True:
        msg = s.recv(48)
        print(msg)
except KeyboardInterrupt:
    pass
finally:
    s.close()
