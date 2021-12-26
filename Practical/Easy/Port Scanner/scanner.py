import socket
from datetime import datetime

target = socket.gethostbyname("www.duckduckgo.com")
print("Scanning started at:", str(datetime.now()))
t1 = datetime.now()
try:
    for port in range(1, 65536):
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        socket.setdefaulttimeout(1)
        result = s.connect_ex((target, port))
        if result == 0:
            print("Port {} is open".format(port))
        s.close()
    print("Time Taken:", datetime.now() - t1)
except KeyboardInterrupt:
    print("Lol peace out")
    print("Time Taken:", datetime.now() - t1)
except socket.gaierror:
    print("Hostname not resolved")
    print("Time Taken:", datetime.now() - t1)
except socket.error:
    print("Unresponsive server")
    print("Time Taken:", datetime.now() - t1)
