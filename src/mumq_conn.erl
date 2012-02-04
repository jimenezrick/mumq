-module(mumq_conn).

-export([handle_connection/2]).

handle_connection(Socket, State) ->
    gen_tcpd:send(Socket, "HELO\n"),
    {ok, Line} = gen_tcpd:recv(Socket, 0),
    io:format("Line = ~s", [Line]),
    handle_connection(Socket, State).


%%% TODO: Tener un buffer de caracteres sin consumir.
%%% TODO: Parsear los frames: {frame, Cmd, {"hname", "htype"}, Body}
%%% TODO: Implementar las transacciones como un envio de un grupo de frames al destinatario?
