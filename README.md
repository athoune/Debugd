Debugd
======

For now, it's a broadcast tool for simple TCP clients

Test
----

	application:start(debugd).

Connect with the client

	./client.py localhost 4807

Back to erlang

	debugd:json('Hello every telnet').