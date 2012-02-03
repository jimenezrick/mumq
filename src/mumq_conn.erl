-module(mumq_conn).

-export([handle_connection/2]).

handle_connection(Socket, _State) ->
    io:format("self = ~p~n", [self()]),
    io:format("$ancestors = ~p~n", [get('$ancestors')]),
    gen_tcpd:send(Socket, "HELO\n").
