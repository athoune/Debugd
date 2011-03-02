-module(broadcaster).
-author("Mathieu Lecarme <mathieu@garambrogne.net>").
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([start_link/0, send_binary/1]).

-export([wait_connect/2]).

-record(state, {socket, port, clients}).

init([]) ->
    Port = 4807,
    case gen_tcp:listen(Port, [binary, {active, once}, {packet, 0}, {reuseaddr, true}]) of
        {ok, Socket} ->
            %spawn(?MODULE, wait_connect, [Socket, 0]),
            {ok, _Ref} = prim_inet:async_accept(Socket, -1),
            {ok, #state{
                socket = Socket,
                port = Port,
                clients = sets:new()
            }};
        {error, Reason} ->
            {stop, Reason}
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%%handle_cast({send, Who, Level, Msg}, State) ->
%%    handle_cast({send, 0, Who, Level, Msg}, State);

handle_cast({broadcast, Msg}, #state{clients=Clients} = State) ->
    sets:fold(fun(E, _AccIn) ->
        gen_tcp:send(E, Msg),
        []
        end, [], Clients),
    io:format("~w says ~s\n~p\n", [self(), Msg, sets:to_list(Clients)]),
    {noreply, State};

handle_cast(_, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp, Sock, Data}, State) ->
    Me = self(),
    P = spawn(fun() -> worker(Me, Sock, Data) end),
    gen_tcp:controlling_process(Sock, P),
    {noreply, State};

handle_info({inet_async, ListSock, _Ref, {ok, CliSocket}} = _Info, #state{clients=Clients, socket=Socket, port=Port} = _State) ->
    %io:format("Info inet async:\n~p\n", [Info]),
    inet_db:register_socket(CliSocket, inet_tcp),
    inet:setopts(CliSocket, [{active, once}]),
    prim_inet:async_accept(ListSock, -1),
    NewClients = sets:add_element(CliSocket, Clients),
    {noreply, #state{clients=NewClients, socket=Socket, port=Port}};

handle_info(Info, State) ->
    io:format("Info:\n~p\n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

wait_connect(ListenSocket, Count) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    {ok, Binary} = gen_tcp:recv(Socket, 0, 5000),
    io:format("recv: ~p", [Binary]),
    wait_connect(ListenSocket, Count+1).

worker(Owner, Sock, _Data) ->
    io:format("Socket : ~p \n", [Sock]),
    %gen_tcp:send(Sock, "Moi je dis " ++ Data),
    inet:setopts(Sock, [{active, once}]),
    gen_tcp:controlling_process(Sock, Owner).

send_binary(Binary) ->
    Size = size(Binary),
    send_raw(iolist_to_binary([<<Size:32/unsigned-integer>> | Binary])).

send_raw(Raw) ->
    gen_server:cast(?MODULE, {broadcast, Raw}).