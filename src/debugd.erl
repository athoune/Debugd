-module(debugd).
-author("Mathieu Lecarme <mathieu@garambrogne.net>").

-export([json/1]).

json(Data) ->
    broadcaster:send_binary(list_to_binary(mochijson2:encode(Data))).
