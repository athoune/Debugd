-module(debugd).
-author("Mathieu Lecarme <mathieu@garambrogne.net>").

-export([message/4, message/1]).

json(Data) ->
    broadcaster:send_data(mochijson2:encode(Data)).

%Use it with ?DEBUGD_MSG macro
message(Module, Line, Process, TagedDatas) ->
    message([
        {module, Module},
        {line, Line},
        {process, io_lib:format("~w", [Process])}
            | TagedDatas]).

message(TagedDatas) ->
    json({struct, TagedDatas}).
