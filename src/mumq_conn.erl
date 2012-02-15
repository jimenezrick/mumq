-module(mumq_conn).

-export([handle_connection/1,
         handle_delivery/2]).

-record(state, {conn_state = disconnected,
                delivery_proc,
                session}).

%%%-----------------------------------------------------------------------------
%%% TODO: Falta añadir el message-id del mensaje en send_frame/2
%%% TODO: Implement SUBSCRIBE
%%%
%%% SUBSCRIBE
%%% destination: /queue/foo
%%% ack: client/auto <----------------------------------- !!!
%%%
%%% ^@
%%%-----------------------------------------------------------------------------

handle_connection(Socket) ->
    Conn = mumq_stomp:create_conn(Socket),
    Pid = spawn_link(?MODULE, handle_delivery, [Socket, mumq_stomp:peername(Conn)]),
    State = #state{delivery_proc = Pid},
    handle_connection(State, Conn).

handle_connection(State, Conn) ->
    try
        case mumq_stomp:read_frame(Conn) of
            {error, bad_frame} ->
                mumq_stomp:write_frame(mumq_stomp:socket(Conn),
                                       mumq_stomp:error_frame("invalid frame")),
                lager:info("Invalid frame received from ~s",
                           [mumq_stomp:peername(Conn)]),
                gen_tcpd:close(mumq_stomp:socket(Conn));
            {error, bad_frame_size} ->
                mumq_stomp:write_frame(mumq_stomp:socket(Conn),
                                       mumq_stomp:error_frame("frame too big")),
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

handle_frame(State = #state{conn_state = disconnected}, Conn, Frame = {frame, <<"CONNECT">>, _, _}) ->
    lager:info("New client from ~s", [mumq_stomp:peername(Conn)]),
    case authenticate_client(Frame) of
        ok ->
            Session = mumq_stomp:make_session_id(),
            mumq_stomp:write_frame(mumq_stomp:socket(Conn), mumq_stomp:connected_frame(Session)),
            lager:info("Client ~s connected with session ~s", [mumq_stomp:peername(Conn), Session]),
            handle_connection(State#state{conn_state = connected, session = Session}, Conn);
        {error, incorrect_login} ->
            mumq_stomp:write_frame(mumq_stomp:socket(Conn), mumq_stomp:error_frame("incorrect login")),
            lager:info("Client ~s failed authentication", [mumq_stomp:peername(Conn)]),
            gen_tcpd:close(mumq_stomp:socket(Conn))
    end;
handle_frame(State = #state{conn_state = connected}, Conn, {frame, <<"DISCONNECT">>, _, _}) ->
    lager:info("Connection closed by ~s with session ~s", [mumq_stomp:peername(Conn), State#state.session]),
    gen_tcpd:close(mumq_stomp:socket(Conn));
handle_frame(State = #state{conn_state = connected}, Conn, Frame = {frame, <<"SEND">>, Headers, _}) ->
    case proplists:get_value(<<"destination">>, Headers) of
        undefined ->
            mumq_stomp:write_frame(mumq_stomp:socket(Conn), mumq_stomp:error_frame("invalid frame")),
            lager:info("Invalid frame received from ~s", [mumq_stomp:peername(Conn)]),
            gen_tcpd:close(mumq_stomp:socket(Conn));
        Dest ->
            Pids = mumq_subs:get_subscribers(Dest),
            lists:foreach(fun(P) -> send_frame(P, Frame) end, Pids),
            handle_connection(State, Conn)
    end;
handle_frame(State = #state{conn_state = connected}, Conn, {frame, <<"SUBSCRIBE">>, Headers, _}) ->
    case proplists:get_value(<<"destination">>, Headers) of
        undefined ->
            mumq_stomp:write_frame(mumq_stomp:socket(Conn), mumq_stomp:error_frame("invalid frame")),
            lager:info("Invalid frame received from ~s", [mumq_stomp:peername(Conn)]),
            gen_tcpd:close(mumq_stomp:socket(Conn));
        Dest ->
            mumq_subs:add_subscriber(Dest, State#state.delivery_proc),
            handle_connection(State, Conn)
    end;
handle_frame(_, Conn, _) ->
    mumq_stomp:write_frame(mumq_stomp:socket(Conn), mumq_stomp:error_frame("invalid command")),
    lager:info("Invalid command received from ~s", [mumq_stomp:peername(Conn)]),
    gen_tcpd:close(mumq_stomp:socket(Conn)).

handle_delivery(Socket, Peer) ->
    % TODO:
    % 2. Terminar este proceso cuando acabe el otro, monitor?
    receive
        Frame ->
            case mumq_stomp:write_frame(Socket, Frame) of
                ok ->
                    handle_delivery(Socket, Peer);
                {error, Reason} ->
                    lager:debug("Couldn't deliver message to ~s", [Peer]),
                    exit(Reason)
            end
    end.

send_frame(Pid, Frame) ->
    {frame, <<"SEND">>, Headers, Body} = Frame,
    Pid ! {frame, <<"MESSAGE">>, Headers, Body}.
