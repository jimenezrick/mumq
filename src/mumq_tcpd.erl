-module(mumq_tcpd).

-behaviour(gen_tcpd).

-export([start_link/3,
         init/1,
         handle_connection/2,
         handle_info/2,
         terminate/2]).

-include("mumq.hrl").

-define(TCP_OPTS, [binary,
                   {active, false},
                   {packet, raw},
                   {reuseaddr, true},
                   {nodelay, true},
                   {keepalive, true}]).

start_link(Name, Type, Port) ->
    gen_tcpd:start_link({local, Name}, ?MODULE, [], Type, Port,
                        [{acceptors, ?TCP_ACCEPTORS},
                         {link_acceptors, false},
                         {socket_options, ?TCP_OPTS}]).

init(_Args) ->
    {ok, none}.

handle_connection(Socket, none) ->
    mumq_conn:handle_connection(Socket).

handle_info(_Info, _State) ->
    noreply.

terminate(_Reason, _State) ->
    ok.
