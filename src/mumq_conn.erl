-module(mumq_conn).

-export([handle_connection/2]).

%%% TODO: Move this code to mumq_tcpd if it is a few LOCS

handle_connection(Socket, none) ->
    Conn = mumq_stomp:create_conn(Socket),
    lager:info("New connection from ~s", [mumq_stomp:peername(Conn)]),
    handle_connection(Socket, Conn);
handle_connection(Socket, Conn) ->
    try
        gen_tcpd:send(Socket, "HELO\n"),
        case mumq_stomp:read_frame(Conn) of
            {error, bad_frame} ->
                lager:info("Invalid frame received from ~s",
                           [mumq_stomp:peername(Conn)]),
                gen_tcpd:close(Socket);
            {error, bad_frame_size} ->
                lager:info("Frame too big received from ~s",
                           [mumq_stomp:peername(Conn)]),
                gen_tcpd:close(Socket);
            {ok, Frame, Conn2} ->
                mumq_stomp:log_frame(Frame, mumq_stomp:peername(Conn2)),
                handle_frame(Socket, Frame),
                handle_connection(Socket, Conn2)
        end
    catch
        throw:tcp_closed ->
            lager:info("Connection closed by ~s", [mumq_stomp:peername(Conn)]);
        throw:tcp_error ->
            lager:info("Connection error with ~s", [mumq_stomp:peername(Conn)])
    end.





%%% TODO: Need a State to handle the session

-record(state, {status = disconnected, session}).

%handle_frame(_Socket, {frame, <<"CONNECT">>, _Headers, _Body}) ->




handle_frame(_Socket, {frame, <<"SUBSCRIBE">>, _Headers, _Body}) ->
    lager:debug(" --- SUBSCRIBE frame ---").
