from socket import *

# マルチキャストアドレス
# 詳しくは[multicast_sender.py](multicast_reciever.py) のドキュメント参考
multicast_group = "239.255.0.1"


multicast_if_addr = "0.0.0.1"
port = 5002

s = socket(AF_INET, SOCK_DGRAM) # Create socket with INET(IPv4), UDP
s.setsockopt(SOL_SOCKET, SO_REUSEADDR, 1) # enable reusing the same address on the same time
s.bind(('', port))

# Create 'ip_mreq' structure (in c)
mreq = inet_aton(multicast_group)+inet_aton(multicast_if_addr)

# これによりMEMBERSHIPにjoinできる。
# そうするとrecvで受け取れるようになる。
s.setsockopt(IPPROTO_IP, IP_ADD_MEMBERSHIP, mreq)

try:
    while True:
        print(s.recv((48)))
except KeyboardInterrupt:
    pass
finally:
    s.close()
