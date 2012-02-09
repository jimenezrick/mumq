-module(mumq_conn).

-export([handle_connection/1]).

handle_connection(Socket) ->
    handle_connection(disconnected, mumq_stomp:create_conn(Socket)).

handle_connection(State, Conn) ->
    try
        case mumq_stomp:read_frame(Conn) of
            {error, bad_frame} ->
                % TODO: Answer with error
                lager:info("Invalid frame received from ~s",
                           [mumq_stomp:peername(Conn)]),
                gen_tcpd:close(mumq_stomp:socket(Conn));
            {error, bad_frame_size} ->
                % TODO: Answer with error
                lager:info("Frame too big received from ~s",
                           [mumq_stomp:peername(Conn)]),
                gen_tcpd:close(mumq_stomp:socket(Conn));
            {ok, Frame, Conn2} ->
                mumq_stomp:log_frame(Frame, mumq_stomp:peername(Conn2)),
                handle_frame(State, Conn2, Frame)
        end
    catch
        throw:tcp_closed ->
            lager:info("Connection closed by ~s", [mumq_stomp:peername(Conn)]);
        throw:tcp_error ->
            lager:info("Connection error with ~s", [mumq_stomp:peername(Conn)])
    end.

authenticate_client({frame, <<"CONNECT">>, Headers, _}) ->
    case application:get_env(allow_users) of
        undefined ->
            ok;
        {ok, UsersList} ->
            case
                {proplists:get_value(<<"login">>, Headers),
                 proplists:get_value(<<"passcode">>, Headers)}
            of
                {A, B} when A == undefined; B == undefined ->
                    {error, incorrect_login};
                {User, Pass} ->
                    Pass2 = binary_to_list(Pass),
                    case proplists:get_value(binary_to_list(User), UsersList) of
                        undefined ->
                            {error, incorrect_login};
                        Pass2 ->
                            ok;
                        _ ->
                            {error, incorrect_login}
                    end
            end
    end.

handle_frame(disconnected, Conn, Frame = {frame, <<"CONNECT">>, _, _}) ->
    lager:info("New client from ~s", [mumq_stomp:peername(Conn)]),
    case authenticate_client(Frame) of
        ok ->
            mumq_stomp:write_frame(mumq_stomp:socket(Conn),
                                   mumq_stomp:connected_frame()),
            lager:info("Client ~s connected", [mumq_stomp:peername(Conn)]),
            handle_connection(connected, Conn);
        {error, incorrect_login} ->
            mumq_stomp:write_frame(mumq_stomp:socket(Conn),
                                   mumq_stomp:error_frame("incorrect login")),
            lager:info("Client ~s failed authentication", [mumq_stomp:peername(Conn)]),
            gen_tcpd:close(mumq_stomp:socket(Conn))
    end;
handle_frame(connected, Conn, {frame, <<"DISCONNECT">>, _, _}) ->
    lager:info("Connection closed by ~s", [mumq_stomp:peername(Conn)]),
    gen_tcpd:close(mumq_stomp:socket(Conn));
handle_frame(State, Conn, _) ->
    mumq_stomp:write_frame(mumq_stomp:socket(Conn),
                           mumq_stomp:error_frame("invalid command")),
    lager:info("Invalid command received from ~s", [mumq_stomp:peername(Conn)]),
    handle_connection(State, Conn).
