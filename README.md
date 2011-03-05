Debugd
======

For now, it's a broadcast tool for simple TCP clients

Test
----

	application:start(debugd).

Connect with the client

	./client.py localhost 4807

Back to erlang

	debugd:message([{msg, 'Hello every telnet'}]).
	
Changelog
---------

__0.2__ macro, hidden serialization, basic api

__0.1__ tcp server, broadcast, protocol, client