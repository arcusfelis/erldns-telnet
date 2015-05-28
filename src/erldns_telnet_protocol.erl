%% @doc Ranch handler for chat clients
%%
%% We do not use `gen_server:start_link/3' to start this gen server.
%% Instead, we use `proc_lib' to enter the gen server loop inside a process,
%% spawned by Ranch.
-module(erldns_telnet_protocol).
-behaviour(gen_server).
-behaviour(ranch_protocol).
-author('arcusfelis@gmail.com'). 

%% API.
-export([start_link/4]).

%% gen_server.
-export([init/1]).
-export([init/4]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(DEFAULT_INACTIVITY_TIMEOUT, infinity).
-define(DEFAULT_SEPARATOR, <<"\r">>).

-record(state, {socket, transport, acc, separator, inactivity_timeout}).

%% API.

start_link(Ref, Socket, Transport, Opts) ->
	proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%% gen_server.

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init([]) ->
    {ok, undefined}.

init(Ref, Socket, Transport, Opts) ->
    Separator = proplists:get_value(message_separator, Opts, ?DEFAULT_SEPARATOR),
    Timeout = proplists:get_value(inactivity_timeout, Opts, ?DEFAULT_INACTIVITY_TIMEOUT),
	ok = proc_lib:init_ack({ok, self()}),
	ok = ranch:accept_ack(Ref),
	ok = Transport:setopts(Socket, [{active, once}]),
    State = #state{socket=Socket, transport=Transport, acc = <<>>,
                   separator=Separator, inactivity_timeout=Timeout},
	gen_server:enter_loop(?MODULE, [], State, Timeout).

handle_info({tcp, Socket, Data}, State=#state{
		socket=Socket, transport=Transport, acc=Acc, separator=Separator,
        inactivity_timeout=Timeout}) ->
    Acc2 = handle_incoming_data(Socket, Data, Acc, Transport, Separator),
    State2 = State#state{acc=Acc2},
	{noreply, State2, Timeout};
handle_info({tcp_closed, Socket}, State) ->
    lager:info("issue=\"TCP closed\", socket=~p", [Socket]),
	{stop, normal, State};
handle_info({tcp_error, Socket, Reason}, State) ->
    lager:info("issue=\"TCP error\", socket=~p, reason=~p", [Socket, Reason]),
	{stop, Reason, State};
handle_info(timeout, State) ->
    lager:info("issue=\"Inactivity timeout, stop\"", []),
	{stop, normal, State};
handle_info(Info, State=#state{inactivity_timeout=Timeout}) ->
    lager:info("issue=\"Unknown info message\", message=~p", [Info]),
	{noreply, State, Timeout}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

handle_incoming_data(Socket, Data, Acc, Transport, Separator)
    when is_binary(Data), is_binary(Acc) ->
    Acc2 = <<Acc/binary, Data/binary>>,
    {Messages, Acc3} = split_messages(Acc2, Separator),
    handle_incoming_messages(Messages),
	Transport:setopts(Socket, [{active, once}]),
    Acc3.

handle_incoming_messages(Messages) ->
    [handle_incoming_message(Message) || Message <- Messages].

handle_incoming_message(Data) ->
    lager:debug("issue=\"Receive data\", data=~p", [Data]),
    %% TODO
    ok.

handle_message_broadcast(Socket, Data, Transport, FromPid) ->
    lager:debug("issue=\"Write message to socket\", from_pid=~p, data=~p",
                [FromPid, Data]),
	Transport:send(Socket, Data),
    ok.


%% We use binary:split/3, because it's fast.
%% But it removes separators, so we should add them back.
%% binary:split/3 returns a list with accumulater as a last element.
split_messages(Data, Separator) ->
    Splitted = binary:split(Data, Separator, [global]),
    convert_splitted_to_list_and_new_acc(Splitted, Separator).

convert_splitted_to_list_and_new_acc(Splitted, Separator) ->
    [Acc2|RevList] = lists:reverse(Splitted),
    List = lists:reverse(RevList),
    List2 = map_add_separator_back(List, Separator),
    {List2, Acc2}.

add_separator_back(Bin, Separator) ->
    <<Bin/binary, Separator/binary>>.

map_add_separator_back(Bins, Separator) ->
    [add_separator_back(Bin, Separator) || Bin <- Bins].
