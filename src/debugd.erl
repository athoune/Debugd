-module(debugd).
-author("Mathieu Lecarme <mathieu@garambrogne.net>").

-export([json/1]).

json(Data) ->
    broadcaster:send_data(mochijson2:encode(Data)).
