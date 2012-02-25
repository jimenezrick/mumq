-module(mumq_client).

-export([connect/1,
         connect_ssl/1,
         connect/3,
         close/1,
         send_frame/2,
         recv_frame/1,
         recv_frame/2]).

-include("mumq.hrl").

connect(Host) ->
    connect(Host, ?TCP_PORT, tcp).

connect_ssl(Host) ->
    connect(Host, ?TCP_PORT + 1, ssl).

connect(Host, Port, Type) ->
    Res = case Type of
        tcp ->
            gen_tcp:connect(Host, Port, ?TCP_OPTS);
        ssl ->
            ssl:connect(Host, Port, ?TCP_OPTS)
    end,
    case Res of
        {ok, Socket} ->
            {ok, mumq_stomp:make_conn(make_socket(Socket, Type))};
        {error, Reason} ->
            {error, Reason}
    end.

make_socket(Socket, tcp)  -> {gen_tcp, Socket};
make_socket(Socket, Type) -> {Type, Socket}.

close(Conn) ->
    mumq_stomp:close_conn(Conn).

send_frame(Conn, Frame) ->
    mumq_stomp:write_frame(Conn, Frame).

recv_frame(Conn) ->
    mumq_stomp:recv_frame(Conn).

recv_frame(Conn, Handlers) ->
    case recv_frame(Conn) of
        {ok, Frame, Conn2} ->
            {ok, dispatch_frame(Frame, Handlers), Conn2};
        {error, Reason} ->
            {error, Reason}
    end.

dispatch_frame(Frame, []) ->
    Frame;
dispatch_frame(Frame, [{Handler, Headers} | Rest]) ->
    case handler_react(Frame#frame.headers, Headers) of
        true ->
            {dispatched, Handler(Frame)};
        false ->
            dispatch_frame(Frame, Rest)
    end.

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
