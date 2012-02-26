-module(mumq_client).

-export([connect/1,
         connect_ssl/1,
         connect/3,
         connect/5,
         disconnect/1,
         subscribe/2,
         subscribe/3,
         unsubscribe/2,
         unsubscribe/3,
         send/3,
         send_frame/2,
         recv/1,
         recv_dispatch/2]).

-include("mumq.hrl").

connect(Host) ->
    connect(Host, ?TCP_PORT, tcp, undefined, undefined).

connect_ssl(Host) ->
    connect(Host, ?TCP_PORT + 1, ssl, undefined, unsubscribe).

connect(Host, Port, Type) ->
    connect(Host, Port, Type, undefined, undefined).

connect(Host, Port, Type, Login, Pass) ->
    Res = case Type of
        tcp ->
            gen_tcp:connect(Host, Port, ?TCP_OPTS);
        ssl ->
            ssl:connect(Host, Port, ?TCP_OPTS)
    end,
    case Res of
        {ok, Socket} ->
            Conn = mumq_stomp:make_conn(make_socket(Socket, Type)),
            if
                Login /= undefined, Pass /= undefined ->
                    mumq_stomp:write_frame(Conn, mumq_stomp:connect_frame(Pass, Login));
                true ->
                    mumq_stomp:write_frame(Conn, mumq_stomp:connect_frame())
            end,
            wait_connected(Conn);
        {error, Reason} ->
            {error, Reason}
    end.

make_socket(Socket, tcp)  -> {gen_tcp, Socket};
make_socket(Socket, Type) -> {Type, Socket}.

wait_connected(Conn) ->
    case recv(Conn) of
        {ok, #frame{cmd = connected}, Conn2} ->
            {ok, Conn2};
        {error, Reason} ->
            {error, Reason}
    end.

disconnect(Conn) ->
    mumq_stomp:write_frame(Conn, mumq_stomp:disconnect_frame()),
    mumq_stomp:close_conn(Conn).

subscribe(Conn, Queue) ->
    mumq_stomp:write_frame(Conn, mumq_stomp:subscribe_frame(Queue)).

subscribe(Conn, Queue, Id) ->
    mumq_stomp:write_frame(Conn, mumq_stomp:subscribe_frame(Queue, Id)).

unsubscribe(Conn, Queue) ->
    mumq_stomp:write_frame(Conn, mumq_stomp:unsubscribe_frame(Queue)).

unsubscribe(Conn, Queue, Id) ->
    mumq_stomp:write_frame(Conn, mumq_stomp:unsubscribe_frame(Queue, Id)).

send(Conn, Queue, Msg) ->
    Frame = mumq_stomp:add_content_length(mumq_stomp:message_frame(Queue, Msg)),
    mumq_stomp:write_frame(Conn, Frame).

send_frame(Conn, Frame) ->
    mumq_stomp:write_frame(Conn, Frame).

recv(Conn) ->
    case mumq_stomp:read_frame(Conn) of
        {ok, Frame = #frame{cmd = error, body = ErrorBody0}, _} ->
            ErrorMsg0 = mumq_stomp:get_header(Frame, "message"),
            ErrorMsg = iolist_to_list(ErrorMsg0),
            ErrorBody = iolist_to_list(ErrorBody0),
            {error, {error_frame, ErrorMsg, ErrorBody}};
        {ok, Frame, Conn2} ->
            {ok, Frame, Conn2};
        {error, Reason} ->
            {error, Reason}
    end.

recv_dispatch(Conn, Handlers) ->
    case recv(Conn) of
        {ok, Frame, Conn2} ->
            {ok, dispatch_frame(Frame, Handlers), Conn2};
        {error, Reason} ->
            {error, Reason}
    end.

dispatch_frame(Frame, []) ->
    Frame;
dispatch_frame(Frame = #frame{cmd = Cmd}, [{Handler, Cmd} | _]) ->
    {dispatched, Handler(Frame)};
dispatch_frame(Frame = #frame{cmd = Cmd}, [{Handler, Cmd, Headers} | Rest]) ->
    case handler_react(Frame#frame.headers, Headers) of
        true ->
            {dispatched, Handler(Frame)};
        false ->
            dispatch_frame(Frame, Rest)
    end;
dispatch_frame(Frame, [_ | Rest]) ->
    dispatch_frame(Frame, Rest).

handler_react(_, []) ->
    true;
handler_react(Headers, [{Key0, Val0} | Rest]) ->
    Key = list_to_binary(Key0),
    Val = list_to_binary(Val0),
    case proplists:get_value(Key, Headers) of
        Val ->
            handler_react(Headers, Rest);
        _ ->
            false
    end;
handler_react(Headers, [Key0 | Rest]) ->
    Key = list_to_binary(Key0),
    case proplists:is_defined(Key, Headers) of
        true ->
            handler_react(Headers, Rest);
        false ->
            false
    end.

iolist_to_list(L) -> binary_to_list(iolist_to_binary(L)).
