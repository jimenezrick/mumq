-module(mumq_conn).

-export([handle_connection/2]).

handle_connection(Socket, _State) ->
    gen_tcpd:send(Socket, "HELO\n"),
    io:format("$ancestors = ~p~n", [get('$ancestors')]),
    timer:sleep(timer:seconds(3)).
