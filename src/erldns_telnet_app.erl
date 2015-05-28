-module(erldns_telnet_app).
-behaviour(application).
-author('arcusfelis@gmail.com'). 

-define(DEFAULT_INACTIVITY_TIMEOUT, infinity).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
    {ok, ListenPort} = application:get_env(erldns_telnet, listen_port),
    InactivityTimeout = get_default_env(erldns_telnet, inactivity_timeout, ?DEFAULT_INACTIVITY_TIMEOUT),
    lager:info("issue=\"start erldns_telnet application\", "
               "listen_port=~p, inactivity_timeout=~p",
               [ListenPort, InactivityTimeout]),
    SocketOpts = [{port, ListenPort}, {packet,raw}, {nodelay, true}],
    ProtocolOpts = [{inactivity_timeout, InactivityTimeout}],
    SupStartRes = erldns_telnet_sup:start_link(),
    %% Start ranch once ready
    NbAcceptors = 10,
    {ok, _} = ranch:start_listener(erldns_telnet, NbAcceptors,
		ranch_tcp, SocketOpts, erldns_telnet_protocol, ProtocolOpts),
    SupStartRes.

stop(_State) ->
	ok.

%% Private functions

%% This is get_env/3 for R15
get_default_env(App, Env, Def) ->
    get_default_env_2(application:get_env(App, Env), Def).

get_default_env_2(undefined, Def) ->
    Def;
get_default_env_2({ok, Value}, _Def) ->
    Value.
