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
    pack(iolist_to_binary(Data));

pack(_Data) ->
    {error, "only list and binary are handled"}.

unpack(Blob) ->
    <<Size:32/unsigned-integer, Data/binary>> = Blob,
    case Size == size(Data) of
        true ->
            io:format("~p ~n ~p", [Size, Data]),
            {ok, Data};
        false ->
            {error, "wrong size"}
    end.

-ifdef(EUNIT).
pack_test() ->
    {error, _} = pack(42),
    {ok, Blob} = pack("hello"),
    {ok, Value} = unpack(Blob),
    ?assertEqual(<<"hello">>, Value).
-endif.