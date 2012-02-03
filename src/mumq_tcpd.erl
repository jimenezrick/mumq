-module(mumq_tcpd).

-behaviour(gen_tcpd).

-export([start_link/0,
         init/1,
         handle_connection/2,
         handle_info/2,
         terminate/2]).

start_link() ->
    % TODO: Add tcp options nodelay and keepalive
    gen_tcpd:start_link({local, ?MODULE}, ?MODULE, [], tcp, 61613, []).

init(_Args) ->
    {ok, none}.

handle_connection(Socket, State) ->
    mumq_conn:handle_connection(Socket, State).

handle_info(_Info, _State) ->
    noreply.

terminate(_Reason, _State) ->
    ok.
