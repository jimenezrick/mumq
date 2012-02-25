-module(mumq_conn).

-export([handle_connection/1,
         handle_delivery/3]).

-include("mumq.hrl").

-record(state, {conn_state = disconnected,
                delivery_proc,
                session}).

handle_connection(Socket) ->
    Conn = mumq_stomp:create_conn(Socket),
    Pid = start_delivery_proc(Socket, mumq_stomp:peername(Conn)),
    State = #state{delivery_proc = Pid},
    handle_connection(State, Conn).

handle_connection(State, Conn) ->
    try mumq_stomp:read_frame(Conn) of
        {error, bad_frame} ->
            close_with_invalid_frame(Conn);
        {error, bad_frame_size} ->
            close_with_frame_too_big(Conn);
        {ok, Frame, Conn2} ->
            mumq_stomp:log_frame(Frame, mumq_stomp:peername(Conn2)),
            handle_frame(State, Conn2, Frame)
    catch
        throw:tcp_closed ->
            lager:info("Connection closed by ~s", [mumq_stomp:peername(Conn)]);
        throw:tcp_error ->
            lager:info("Connection error with ~s", [mumq_stomp:peername(Conn)])
    end.

authenticate_client(Login, Passcode) ->
    case application:get_env(allow_users) of
        undefined ->
            ok;
        {ok, UsersList} ->
            case {Login, Passcode} of
                {A, B} when A == undefined; B == undefined ->
                    {error, incorrect_login};
                _ ->
                    Passcode2 = binary_to_list(Passcode),
                    case proplists:get_value(binary_to_list(Login), UsersList) of
                        undefined ->
                            {error, incorrect_login};
                        Passcode2 ->
                            ok;
                        _ ->
                            {error, incorrect_login}
                    end
            end
    end.

handle_frame(State = #state{conn_state = connected}, Conn, Frame = #frame{cmd = send}) ->
    case validate_destination(Frame) of
        {ok, Dest} ->
            MsgFrame = mumq_stomp:message_frame(Frame),
            mumq_pers:enqueue_message(Dest, MsgFrame),
            Subs = mumq_subs:get_subscriptions(Dest),
            lists:foreach(
                fun({I, D}) ->
                        D ! mumq_stomp:add_header(MsgFrame, <<"subscription">>, I)
                end, Subs),
            handle_connection(State, Conn);
        {error, _} ->
            close_with_invalid_frame(Conn, Frame)
    end;
handle_frame(State = #state{conn_state = connected}, Conn, Frame = #frame{cmd = subscribe}) ->
    case validate_destination(Frame) of
        {ok, Dest} ->
            SubId = mumq_stomp:get_header(Frame, <<"id">>),
            case mumq_subs:add_subscription(Dest, SubId, State#state.delivery_proc) of
                true when SubId /= undefined ->
                    mumq_pers:send_unread_messages(Dest, SubId, State#state.delivery_proc);
                true ->
                    true;
                false ->
                    write_already_subscribed(Conn, Dest)
            end,
            handle_connection(State, Conn);
        {error, _} ->
            close_with_invalid_frame(Conn, Frame)
    end;
handle_frame(State = #state{conn_state = connected}, Conn, Frame = #frame{cmd = unsubscribe}) ->
    case validate_destination(Frame) of
        {ok, Dest} ->
            SubId = mumq_stomp:get_header(Frame, <<"id">>),
            case mumq_subs:del_subscription(Dest, SubId, State#state.delivery_proc) of
                true ->
                    true;
                false ->
                    write_not_subscribed(Conn, Dest)
            end,
            handle_connection(State, Conn);
        {error, _} ->
            close_with_invalid_frame(Conn, Frame)
    end;
handle_frame(State = #state{conn_state = disconnected}, Conn, Frame = #frame{cmd = connect}) ->
    lager:info("New client from ~s", [mumq_stomp:peername(Conn)]),
    Login = mumq_stomp:get_header(Frame, <<"login">>),
    Passcode = mumq_stomp:get_header(Frame, <<"passcode">>),
    case authenticate_client(Login, Passcode) of
        ok ->
            Session = mumq_stomp:make_uuid_base64(),
            mumq_stomp:write_frame(mumq_stomp:socket(Conn),
                                   mumq_stomp:connected_frame(Session)),
            lager:info("Client ~s connected with session ~s",
                       [mumq_stomp:peername(Conn), Session]),
            handle_connection(State#state{conn_state = connected,
                                          session = Session}, Conn);
        {error, incorrect_login} ->
            close_with_incorrect_login(Conn)
    end;
handle_frame(State = #state{conn_state = connected}, Conn, #frame{cmd = disconnect}) ->
    lager:info("Connection closed by ~s with session ~s",
               [mumq_stomp:peername(Conn), State#state.session]),
    gen_tcpd:close(mumq_stomp:socket(Conn));
handle_frame(_, Conn, Frame) ->
    close_with_invalid_frame(Conn, Frame).

validate_destination(Frame) ->
    case mumq_stomp:get_header(Frame, <<"destination">>) of
        undefined ->
            {error, undefined};
        Dest ->
            case
                {binary:first(Dest), binary:last(Dest),
                 binary:match(Dest, <<"//">>)}
            of
                {$/, Last, nomatch} when Last /= $/ ->
                    {ok, Dest};
                _ ->
                    {error, bad_destination}
            end
    end.

start_delivery_proc(Socket, Peer) ->
    proc_lib:start_link(?MODULE, handle_delivery, [self(), Socket, Peer]).

handle_delivery(Parent, Socket, Peer) when is_pid(Parent) ->
    MonitorRef = monitor(process, Parent),
    proc_lib:init_ack(self()),
    handle_delivery(MonitorRef, Socket, Peer);
handle_delivery(MonitorRef, Socket, Peer) ->
    receive
        {'DOWN', MonitorRef, process, _, Reason} ->
            exit(Reason);
        Frame ->
            case mumq_stomp:write_frame(Socket, Frame) of
                ok ->
                    handle_acknowledge(Frame),
                    lager:debug("Message delivered to ~s", [Peer]),
                    handle_delivery(MonitorRef, Socket, Peer);
                {error, Reason} ->
                    lager:debug("Couldn't deliver message to ~s", [Peer]),
                    exit(Reason)
            end
    end.

handle_acknowledge(Frame) ->
    case mumq_stomp:get_header(Frame, <<"subscription">>) of
        undefined ->
            ok;
        SubId ->
            MsgId = mumq_stomp:get_header(Frame, <<"message-id">>),
            Dest = mumq_stomp:get_header(Frame, <<"destination">>),
            mumq_pers:acknowledge_message(Dest, SubId, MsgId)
    end.

write_error_frame(Conn, ErrorMsg, LogMsg) ->
    mumq_stomp:write_frame(mumq_stomp:socket(Conn),
                           mumq_stomp:error_frame(ErrorMsg)),
    lager:info(LogMsg, [mumq_stomp:peername(Conn)]).

write_error_frame(Conn, ErrorMsg, ErrorFrame, LogMsg) ->
    ErrorFrame2 = mumq_stomp:serialize_frame(ErrorFrame),
    ErrorBody = ["--------\n", ErrorFrame2, "\n--------\n"],
    mumq_stomp:write_frame(mumq_stomp:socket(Conn),
                           mumq_stomp:error_frame(ErrorMsg, ErrorBody)),
    lager:info(LogMsg, [mumq_stomp:peername(Conn)]).

close_with_error_frame(Conn, ErrorMsg, LogMsg) ->
    write_error_frame(Conn, ErrorMsg, LogMsg),
    gen_tcpd:close(mumq_stomp:socket(Conn)).

close_with_error_frame(Conn, ErrorMsg, ErrorFrame, LogMsg) ->
    write_error_frame(Conn, ErrorMsg, ErrorFrame, LogMsg),
    gen_tcpd:close(mumq_stomp:socket(Conn)).

write_already_subscribed(Conn, Dest) ->
    write_error_frame(Conn, ["already subscribed to ", Dest],
                      "Client ~s already subscribed to " ++ binary_to_list(Dest)).

write_not_subscribed(Conn, Dest) ->
    write_error_frame(Conn, ["not subscribed to ", Dest],
                      "Client ~s not subscribed to " ++ binary_to_list(Dest)).

close_with_invalid_frame(Conn) ->
    close_with_error_frame(Conn, "invalid frame",
                           "Invalid frame received from ~s").

close_with_invalid_frame(Conn, ErrorFrame) ->
    close_with_error_frame(Conn, "invalid frame", ErrorFrame,
                           "Invalid frame received from ~s").

close_with_frame_too_big(Conn) ->
    close_with_error_frame(Conn, "frame too big",
                           "Frame too big received from ~s").

close_with_incorrect_login(Conn) ->
    close_with_error_frame(Conn, "incorrect login",
                           "Client ~s failed authentication").
