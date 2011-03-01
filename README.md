Debugd
======

For now, it's a broadcast tool for clients connected with telnet

Test
----

	application:start(debugd).

Connect with some telnet

	telnet localhost 4807

Back to erlang

	debugd:json('Hello every telnet').