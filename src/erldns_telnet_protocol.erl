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
-define(DEFAULT_SEPARATOR, [<<"\r\n">>, <<"\r">>, <<"\n">>]).
-define(DEFAULT_ERDNS_NODE, erldns). % node name

-record(state, {socket, transport, acc, separator, inactivity_timeout, erldns_node}).

%% API.

start_link(Ref, Socket, Transport, Opts) ->
	proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%% gen_server.

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init([]) ->
    {ok, undefined}.

init(Ref, Socket, Transport, Opts) ->
    Separator = proplists:get_value(command_separator, Opts, ?DEFAULT_SEPARATOR),
    Timeout = proplists:get_value(inactivity_timeout, Opts, ?DEFAULT_INACTIVITY_TIMEOUT),
    ErldnsNode = proplists:get_value(erldns_node, Opts, ?DEFAULT_ERDNS_NODE),
    ErldnsNode2 = prepare_erldns_node_name(ErldnsNode),
	ok = proc_lib:init_ack({ok, self()}),
	ok = ranch:accept_ack(Ref),
	ok = Transport:setopts(Socket, [{active, once}]),
    State = #state{socket=Socket, transport=Transport, acc = <<>>,
                   separator=Separator, inactivity_timeout=Timeout,
                   erldns_node=ErldnsNode2},
    lager:info("issue=\"Open TCP connection\", socket=~p, erldns_node=~p", [Socket, ErldnsNode2]),
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
handle_info({register_name, DnsName, IpAddress}, State=#state{erldns_node=ErldnsNode}) ->
    lager:info("issue=\"Handle register name command\", name=~p, ip=~p",
               [DnsName, IpAddress]),
    handle_register_name(ErldnsNode, DnsName, IpAddress),
	{stop, normal, State};
handle_info(timeout, State) ->
    lager:info("issue=\"Inactivity timeout, stop\"", []),
	{stop, normal, State};
handle_info(Info, State=#state{inactivity_timeout=Timeout}) ->
    lager:info("issue=\"Unknown info command\", command=~p", [Info]),
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

%% Parsing
%% -------

handle_incoming_data(Socket, Data, Acc, Transport, Separator)
    when is_binary(Data), is_binary(Acc) ->
    Acc2 = <<Acc/binary, Data/binary>>,
    {Messages, Acc3} = split_commands(Acc2, Separator),
    handle_incoming_commands(Messages),
	Transport:setopts(Socket, [{active, once}]),
    Acc3.

handle_incoming_commands(Messages) ->
    [handle_incoming_command(Message) || Message <- Messages].

handle_incoming_command(Data) ->
    lager:debug("issue=\"Receive data\", data=~p", [Data]),
    Args1 = binary:split(Data, <<" ">>, [global]),
    Args2 = skip_empty(Args1),
    handle_incoming_command_2(Args2),
    ok.

%% The order of arguments matters
handle_incoming_command_2([<<"REGISTER">>, <<"name">>, DnsName, <<"ip">>, IpAddress| _]) ->
    send_register_name(DnsName, IpAddress);
handle_incoming_command_2(Args) ->
    lager:error("issue=\"Ignore command\", data=~p", [Args]),
    ok.

%% We use binary:split/3, because it's fast.
%% But it removes separators, so we should add them back.
%% binary:split/3 returns a list with accumulater as a last element.
split_commands(Data, Separator) ->
    Splitted = binary:split(Data, Separator, [global]),
    convert_splitted_to_list_and_new_acc(Splitted, Separator).

convert_splitted_to_list_and_new_acc(Splitted, Separator) ->
    [Acc2|RevList] = lists:reverse(Splitted),
    List = lists:reverse(RevList),
    {List, Acc2}.

skip_empty(List) ->
    [X || X <- List, X =/= <<>>].

send_register_name(DnsName, IpAddress) ->
    self() ! {register_name, DnsName, IpAddress}.


%% Register zone
%% -------------

handle_register_name(ErldnsNode, DnsName, IpAddress) ->
    Binary = make_register_name_json_binary(DnsName, IpAddress),
    JsonZone = remote_jsx_decode(ErldnsNode, Binary),
    Zone = remote_parse_node(ErldnsNode, JsonZone),
    ok = remote_put_zone(ErldnsNode, Zone).

remote_jsx_decode(ErldnsNode, Binary) ->
    rpc_call(ErldnsNode, jsx, decode, [Binary]).

remote_parse_node(ErldnsNode, JsonZone) ->
    rpc_call(ErldnsNode, erldns_zone_parser, zone_to_erlang, [JsonZone]).

remote_put_zone(ErldnsNode, Zone) ->
    rpc_call(ErldnsNode, erldns_zone_cache, put_zone, [Zone]).

rpc_call(Node, Module, Function, Name) ->
    %% TODO handle errors
    rpc:call(Node, Module, Function, Name).


make_register_name_json_binary(DnsName, IpAddress) ->
    %% It's very naive
    <<"{
    \"name\": \"", DnsName/binary, "\",
    \"records\": [
    {
      \"name\": \"", DnsName/binary, "\",
      \"type\": \"SOA\",
      \"ttl\": 3600,
      \"data\": {
        \"serial\": 1,
        \"refresh\": 86400,
        \"retry\": 7200,
        \"expire\": 604800,
        \"minimum\": 300
      }
    },
    {
      \"name\": \"", DnsName/binary, "\",
      \"type\": \"A\",
      \"ttl\": 3600,
      \"data\": {\"ip\": \"", IpAddress/binary, "\"}
    }]
    }">>.

%% Convert dyndns to dyndns@hostname
%% Keep dyndns@other_hostname as it is
prepare_erldns_node_name(ErldnsNode) ->
    MyNode = node(),
    prepare_erldns_node_name(ErldnsNode, MyNode).

prepare_erldns_node_name(ErldnsNode, MyNode) ->
    ErldnsNodeParts = split_node_name(ErldnsNode),
    MyNodeParts = split_node_name(MyNode),
    prepare_erldns_node_name_parts(ErldnsNode, ErldnsNodeParts, MyNodeParts).

split_node_name(Node) when is_atom(Node) ->
    split_node_name(atom_to_binary2(Node));
split_node_name(Node) ->
    binary:split(Node, <<"@">>, []).

prepare_erldns_node_name_parts(ErldnsNode, [ErldnsNodeName], [_MyNodeName, MyNodeHost]) ->
    %% Possible atom leak, but safe, because ErldnsNode is received from config
    concat_full_node_name(ErldnsNodeName, MyNodeHost);
prepare_erldns_node_name_parts(ErldnsNode, _ErldnsNodeParts, _MyNodeParts) ->
    %% Use specified node as is
    ErldnsNode.

%% We love R13...
binary_to_atom2(B) ->
    L = binary_to_list(B),
    list_to_atom(L).

atom_to_binary2(A) ->
    L = atom_to_list(A),
    list_to_binary(L).

concat_full_node_name(Name, Host) ->
    Io = [Name, $@, Host],
    binary_to_atom2(iolist_to_binary(Io)).
