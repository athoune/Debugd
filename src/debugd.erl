-module(debugd).
-author("Mathieu Lecarme <mathieu@garambrogne.net>").

-export([json/1]).

json(Data) ->
    Json = list_to_binary(mochijson2:encode(Data)),
    Size = size(Json),
    io:format("~p : ~p", [Json, Size]),
    broadcaster:rawText(iolist_to_binary([<<Size:32/unsigned-integer>> | Json])).
