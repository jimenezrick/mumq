-module(mumq_conn).

-export([handle_connection/2]).

handle_connection(Socket, State) ->
    gen_tcpd:send(Socket, "HELO\n"),
    {ok, Line} = gen_tcpd:recv(Socket, 0),
    io:format("Line = ~s", [Line]),
    handle_connection(Socket, State).
