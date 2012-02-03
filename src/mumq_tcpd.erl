-module(mumq_tcpd).

-behaviour(gen_tcpd).

-export([start_link/0,
         init/1,
         handle_connection/2,
         handle_info/2,
         terminate/2]).

start_link() ->
    gen_tcpd:start_link({local, ?MODULE}, ?MODULE, [], tcp, 61613,
                        [{socket_options, [{reuseaddr, true},
                                           {nodelay, true},
                                           {keepalive, true}]}]).

init(_Args) ->
    {ok, none}.

handle_connection(Socket, State) ->
    mumq_conn:handle_connection(Socket, State).

handle_info(_Info, _State) ->
    noreply.

terminate(_Reason, _State) ->
    ok.
