#!/usr/bin/env python

import socket
import struct
import json

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(('localhost', 4807))
#s.send('')
while 1:
	size = struct.unpack("!I", s.recv(4))[0]
	data = json.loads(s.recv(size))
	print data
