#!/usr/bin/env python
# -*- coding: utf8 -*-

"""
Simple client for debugd
"""

__author__  = "mathieu@garambrogne.net"

import socket
import struct
import json
import sys

if len(sys.argv) > 1:
	host = sys.argv[1]
else:
	host = 'localhost'
if len(sys.argv) > 2:
	port = int(sys.argv[2])
else:
	port = 4807

print "Connecting %s:%i\n" % (host, port)

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect((host, port))
#s.send('')
while 1:
	size = struct.unpack("!I", s.recv(4))[0]
	data = json.loads(s.recv(size))
	print data
