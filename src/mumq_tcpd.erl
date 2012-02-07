-module(mumq_tcpd).

-behaviour(gen_tcpd).

-export([start_link/2,
         init/1,
         handle_connection/2,
         handle_info/2,
         terminate/2]).

-define(TCP_OPTS, [binary,
                   {active, false},
                   {packet, raw},
                   {reuseaddr, true},
                   {nodelay, true},
                   {keepalive, true}]).

start_link(Type, Port) ->
    gen_tcpd:start_link({local, ?MODULE}, ?MODULE, [], Type, Port,
                        [{acceptors, 10}, {socket_options, ?TCP_OPTS}]).

init(_Args) ->
    {ok, none}.

handle_connection(Socket, State) ->
    mumq_conn:handle_connection(Socket, State).

handle_info(_Info, _State) ->
    noreply.

terminate(_Reason, _State) ->
    ok.
