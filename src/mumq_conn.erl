-module(mumq_conn).

-export([handle_connection/1,
         handle_delivery/3]).

-record(state, {conn_state = disconnected,
                delivery_proc,
                session}).

%%%-----------------------------------------------------------------------------
%%% TODO: Falta añadir el message-id del mensaje en send_frame/2, usar UUID?
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
    Pid = start_delivery_proc(Socket, mumq_stomp:peername(Conn)),
    State = #state{delivery_proc = Pid},
    handle_connection(State, Conn).

handle_connection(State, Conn) ->
    try
        case mumq_stomp:read_frame(Conn) of
            {error, bad_frame} ->
                write_invalid_frame(Conn);
            {error, bad_frame_size} ->
                write_frame_too_big(Conn);
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

handle_frame(State = #state{conn_state = connected}, Conn, Frame = {frame, <<"SEND">>, Headers, _}) ->
    case get_destination(Headers) of
        {ok, Dest} ->
            Pids = mumq_subs:get_subscriptions(Dest),
            lists:foreach(fun(P) -> send_frame(P, Frame) end, Pids),
            handle_connection(State, Conn);
        {error, _} ->
            write_invalid_frame(Conn)
    end;
handle_frame(State = #state{conn_state = connected}, Conn, {frame, <<"SUBSCRIBE">>, Headers, _}) ->
    case get_destination(Headers) of
        {ok, Dest} ->
            mumq_subs:add_subscription(Dest, State#state.delivery_proc),
            handle_connection(State, Conn);
        {error, _} ->
            write_invalid_frame(Conn)
    end;
handle_frame(State = #state{conn_state = disconnected}, Conn, Frame = {frame, <<"CONNECT">>, _, _}) ->
    lager:info("New client from ~s", [mumq_stomp:peername(Conn)]),
    case authenticate_client(Frame) of
        ok ->
            Session = mumq_stomp:make_session_id(),
            mumq_stomp:write_frame(mumq_stomp:socket(Conn), mumq_stomp:connected_frame(Session)),
            lager:info("Client ~s connected with session ~s", [mumq_stomp:peername(Conn), Session]),
            handle_connection(State#state{conn_state = connected, session = Session}, Conn);
        {error, incorrect_login} ->
            write_incorrect_login(Conn)
    end;
handle_frame(State = #state{conn_state = connected}, Conn, {frame, <<"DISCONNECT">>, _, _}) ->
    lager:info("Connection closed by ~s with session ~s", [mumq_stomp:peername(Conn), State#state.session]),
    gen_tcpd:close(mumq_stomp:socket(Conn));
handle_frame(_, Conn, _) ->
    write_invalid_frame(Conn).

get_destination(Headers) ->
    case proplists:get_value(<<"destination">>, Headers) of
        undefined ->
            {error, undefined};
        <<>> ->
            {error, invalid};
        <<$/>> ->
            {ok, <<$/>>};
        Dest ->
            case {binary:first(Dest), binary:last(Dest), binary:match(Dest, <<"//">>)} of
                {$/, Last, nomatch} when Last /= $/ ->
                    {ok, Dest};
                _ ->
                    {error, invalid}
            end
    end.

start_delivery_proc(Socket, Peer) ->
    spawn_link(?MODULE, handle_delivery, [self(), Socket, Peer]).

handle_delivery(Parent, Socket, Peer) when is_pid(Parent) ->
    handle_delivery(monitor(process, Parent), Socket, Peer);
handle_delivery(MonitorRef, Socket, Peer) ->
    receive
        {'DOWN', MonitorRef, process, _, Reason} ->
            exit(Reason);
        Frame ->
            case mumq_stomp:write_frame(Socket, Frame) of
                ok ->
                    lager:debug("Message delivered to ~s", [Peer]),
                    handle_delivery(MonitorRef, Socket, Peer);
                {error, Reason} ->
                    lager:debug("Couldn't deliver message to ~s", [Peer]),
                    exit(Reason)
            end
    end.

send_frame(Pid, Frame) ->
    {frame, <<"SEND">>, Headers, Body} = Frame,
    Pid ! {frame, <<"MESSAGE">>, Headers, Body}.

write_error_frame(Conn, ErrorMsg, LogMsg) ->
    mumq_stomp:write_frame(mumq_stomp:socket(Conn), mumq_stomp:error_frame(ErrorMsg)),
    lager:info(LogMsg, [mumq_stomp:peername(Conn)]),
    gen_tcpd:close(mumq_stomp:socket(Conn)).

write_invalid_frame(Conn) ->
    write_error_frame(Conn, "invalid frame", "Invalid frame received from ~s").

write_frame_too_big(Conn) ->
    write_error_frame(Conn, "frame too big", "Frame too big received from ~s").

write_incorrect_login(Conn) ->
    write_error_frame(Conn, "incorrect login", "Client ~s failed authentication").
