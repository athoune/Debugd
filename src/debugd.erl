-module(debugd).
-author("Mathieu Lecarme <mathieu@garambrogne.net>").

-export([json/1]).

json(Data) ->
    broadcaster:rawText(mochijson2:encode(Data)).
