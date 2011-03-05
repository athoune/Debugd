-module(debugd).
-author("Mathieu Lecarme <mathieu@garambrogne.net>").

-export([message/4, message/1]).

%Use it with ?DEBUGD_MSG macro
message(Module, Line, Process, TagedDatas) ->
    message([
        {module, Module},
        {line, Line},
        {process, iolist_to_binary(io_lib:format("~w", [Process]))}
            | TagedDatas]).

message(TagedDatas) ->
    json({struct, TagedDatas}).

json(Data) ->
    broadcaster:send_data(mochijson2:encode(Data)).
