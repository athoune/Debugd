-module(serialize).
-author("Mathieu Lecarme <mathieu@garambrogne.net>").

-export([
    pack/1,
    unpack/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.



pack(Data) when is_binary(Data) ->
    Size = size(Data),
    {ok, <<Size:32/unsigned-integer, Data/binary>>};

pack(Data) when is_list(Data) ->
    pack(iolist_to_binary(Data)).

unpack(Blob) ->
    <<Size:32/unsigned-integer, Data>> = Blob,
    io:format("~p ~n ~p", [Size, Data]).

-ifdef(EUNIT).
pack_test() ->
    Blob = pack("hello"),
    io:format("~p", [Blob]).
-endif.