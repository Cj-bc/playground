# 参考:
#   - https://memo.saitodev.com/home/python_network_programing/
#   - https://karasuyamatengu.hatenadiary.org/entry/20120511/1336756907
#   - https://docs.python.org/ja/3/library/socket.html#module-socket
#   - `man ip'
#   - `man icmp'
#   - `man socket'
#   - `man setsockopt'
#   - マスタリング TCP/IP 入門編第五版

from socket import *

# マルチキャストアドレス
# 先頭から4bitが b'1110' であるアドレスがマルチキャストアドレスになる
# つまり
#   11100000.00000000.00000000.00000000
#   ...
#   11101111.11111111.11111111.11111111
# まで、
#   224.0.0.0
#   ...
#   239.255.255.255
# の間の数
#
# 【マスタリングTCP/IP 4.3.5 IPマルチキャスト】
multicast_group = "239.255.0.1"

# 送信元として使用するネットワークインターフェースのipアドレス
# ipアドレスは__ネットワークインターフェースごとに一つ以上割り当てられる__ので
# インターフェースを指定する時に使うことができる。
#
# 【マスタリングTCP/IP 4.2 IPの基礎知識】
multicast_if_addr = "0.0.0.1"

# 使用するポート番号
port = 5002


# ソケットを作成
#   - AF_INET: IPv4
#   - SOCK_DGRAM: UDP
s = socket(AF_INET, SOCK_DGRAM)

# s.bindする必要はない。

# ブロードキャストを有効化
s.setsockopt(SOL_SOCKET, SO_BROADCAST, 1) # Enable broadcast

# # データが自身のIFへループバックしてくるか否かを決める
# #   0: ループバックしない; 1: ループバックする(デフォルト)
# # 不要な場合、ループバックしないようにすると負荷を減らせる
# # 現在はsenderとreciever共に同じIFを使っているので、ループバックしてくる必要がある
# # (そうでないとrecieverが受け取れない)
# s.setsockopt(IPPROTO_IP, IP_MULTICAST_LOOP, 0)
s.setsockopt(IPPROTO_IP, IP_MULTICAST_IF, inet_aton(multicast_if_addr))


try:
    c=0
    while True:
        # マルチキャストアドレスに対して送る。ここは普通のUDPと同じ
        s.sendto(b"hello message: " + str(c).encode(), (multicast_group, port))
        c+=1
except KeyboardInterrupt:
    pass
finally:
    print("closing socket")
    s.close()
    print("socket closed")
