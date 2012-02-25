-module(mumq_stomp).

-on_load(save_startup_timestamp/0).

-export([create_conn/1,
         socket/1,
         peername/1,
         write_frame/2,
         read_frame/1,
         log_frame/2]).

-export([connect_frame/0,
         connect_frame/2,
         connected_frame/1,
         disconnect_frame/0,
         message_frame/1,
         message_frame/2,
         error_frame/1,
         error_frame/2,
         add_header/3,
         add_content_length/1,
         get_header/2,
         serialize_frame/1,
         make_uuid/0,
         make_uuid_base64/0]).

-include("mumq.hrl").

-define(IS_BLANK_GUARD(X), X == $ ; X == $\t; X == $\r).
-define(HASH_RANGE, 4294967296).

-record(conn, {sock,
               peer,
               recv_len,
               max_frame_size,
               frame_size = 0,
               buf = []}).

create_conn(Socket) ->
    {ok, Peer0} = gen_tcpd:peername(Socket),
    Peer = format_peer(Peer0),
    {ok, [{recbuf, RecvLen}]} = gen_tcpd:getopts(Socket, [recbuf]),
    Max = max_frame_size(),
    #conn{sock = Socket, peer = Peer, recv_len = RecvLen, max_frame_size = Max}.

format_peer({{O1, O2, O3, O4}, P}) ->
    io_lib:format("~B.~B.~B.~B:~B", [O1, O2, O3, O4, P]).

max_frame_size() ->
    case application:get_env(max_frame_size) of
        undefined ->
            ?MAX_FRAME_SIZE;
        {ok, Max} ->
            Max
    end.

socket(Conn) -> Conn#conn.sock.

peername(Conn) -> Conn#conn.peer.

write_frame(Socket, Frame) ->
    gen_tcpd:send(Socket, serialize_frame(Frame)).

prepare_headers(Headers) ->
    [prepare_header({Key, Val}) || {Key, Val} <- Headers, Val /= undefined].

prepare_header({Key, Val}) when is_list(Key) ->
    prepare_header({list_to_binary(Key), Val});
prepare_header({<<"content-length">>, Len}) when is_integer(Len) ->
    prepare_header({<<"content-length">>, integer_to_list(Len)});
prepare_header({Key, Val}) ->
    [Key, ":", Val, "\n"].

read_frame(Conn) ->
    try
        read_frame2(Conn)
    catch
        throw:bad_frame ->
            {error, bad_frame};
        throw:bad_frame_size ->
            {error, bad_frame_size}
    end.

read_frame2(Conn) ->
    Conn2 = eat_empty_lines(Conn),
    {Cmd, Conn3} = read_command(Conn2),
    {Headers, Conn4} = read_headers(Conn3),
    BodySize = proplists:get_value(<<"content-length">>, Headers),
    {Body, Conn5} = read_body(Conn4, BodySize),
    {ok, #frame{cmd = Cmd, headers = Headers, body = Body}, Conn5}.

read_command(Conn) ->
    {Line, Conn2} = read_line(Conn),
    Cmd = case Line of
        <<"CONNECT">> ->
            connect;
        <<"SEND">> ->
            send;
        <<"SUBSCRIBE">> ->
            subscribe;
        <<"UNSUBSCRIBE">> ->
            unsubscribe;
        <<"BEGIN">> ->
            'begin';
        <<"COMMIT">> ->
            commit;
        <<"ABORT">> ->
            abort;
        <<"ACK">> ->
            ack;
        <<"DISCONNECT">> ->
            disconnect;
        <<"MESSAGE">> ->
            message;
        <<"RECEIPT">> ->
            receipt;
        <<"ERROR">> ->
            error;
        _ ->
            throw(bad_frame)
    end,
    {Cmd, Conn2}.

read_headers(Conn) ->
    try
        {Headers, Conn2} = read_headers(Conn, []),
        {parse_headers(Conn2, Headers), Conn2}
    catch
        error:_ ->
            throw(bad_frame)
    end.

read_headers(Conn, Headers) ->
    case read_line(Conn) of
        {<<>>, Conn2} ->
            {lists:reverse(Headers), Conn2};
        {Line, Conn2} ->
            [Key, Val] = lists:map(fun strip_blanks/1, binary:split(Line, <<":">>)),
            no_empty_binary(Key),
            no_empty_binary(Val),
            read_headers(Conn2, [{Key, Val} | Headers])
    end.

no_empty_binary(<<>>) ->
    throw(bad_frame);
no_empty_binary(_) ->
    true.

strip_blanks(<<H, T/binary>>) when ?IS_BLANK_GUARD(H) ->
    strip_blanks(T);
strip_blanks(Bin) ->
    strip_blanks_right(Bin, [], []).

strip_blanks_right(<<>>, _, Acc) ->
    list_to_binary(lists:reverse(Acc));
strip_blanks_right(<<H, T/binary>>, Keep, Acc) when ?IS_BLANK_GUARD(H) ->
    strip_blanks_right(T, [H | Keep], Acc);
strip_blanks_right(<<H, T/binary>>, Keep, Acc) ->
    strip_blanks_right(T, [], [H | Keep ++ Acc]).

parse_headers(Conn, Headers) ->
    [parse_header(Conn, H) || H <- Headers].

parse_header(Conn, {<<"content-length">>, Len}) ->
    case list_to_integer(binary_to_list(Len)) of
        Len2 when Len2 + Conn#conn.frame_size > Conn#conn.max_frame_size ->
            throw(bad_frame_size);
        Len2 ->
            true
    end,
    {<<"content-length">>, Len2};
parse_header(_, Header) ->
    Header.

read_line(Conn) ->
    Concat = fun(B1, B2) -> <<B2/binary, B1/binary>> end,
    {Chunk, Conn2} = read_chunk(Conn, <<"\n">>),
    {lists:foldl(Concat, <<>>, Chunk), Conn2}.

read_body(Conn, undefined) ->
    read_chunk(Conn, <<0>>);
read_body(Conn, Size) ->
    read_size_chunk(Conn, Size, <<0>>).

eat_empty_lines(Conn) ->
    case peek_byte(Conn) of
        {<<"\n">>, Conn2} ->
            eat_empty_lines(eat_byte(Conn2));
        {_, Conn2} ->
            Conn2
    end.

read_chunk(Conn, Sep) ->
    {Parts, Conn2} = read_chunk(Conn, Sep, []),
    {lists:reverse(Parts), Conn2}.

read_chunk(Conn, Sep, Parts) ->
    [Data | Rest] = read_buffer(Conn),
    Size = Conn#conn.frame_size,
    case binary:split(Data, Sep) of
        L when size(hd(L)) + Size > Conn#conn.max_frame_size ->
            throw(bad_frame_size);
        [Part] ->
            read_chunk(Conn#conn{buf = Rest, frame_size = Size + size(Part)},
                       Sep, [Part | Parts]);
        [<<>>, <<>>] ->
            {[<<>> | Parts], Conn#conn{buf = Rest}};
        [<<>>, MoreData] ->
            {[<<>> | Parts], Conn#conn{buf = [MoreData | Rest]}};
        [Part, <<>>] ->
            {[Part | Parts], Conn#conn{buf = Rest, frame_size = Size + size(Part)}};
        [Part, MoreData] ->
            {[Part | Parts], Conn#conn{buf = [MoreData | Rest],
                                       frame_size = Size + size(Part)}}
    end.

read_size_chunk(Conn, Size, Sep) ->
    {Parts, Conn2} = read_size_chunk(Conn, Size, Sep, []),
    case peek_byte(Conn2) of
        {Sep, Conn3} ->
            {lists:reverse(Parts), eat_byte(Conn3)};
        _ ->
            throw(bad_frame)
    end.

read_size_chunk(Conn, 0, _, Parts) ->
    {Parts, Conn};
read_size_chunk(Conn, Size, Sep, Parts) ->
    [Data | Rest] = read_buffer(Conn, min(Size, Conn#conn.recv_len)),
    FSize = Conn#conn.frame_size,
    case size(Data) of
        N when N =< Size ->
            Conn2 = Conn#conn{buf = Rest, frame_size = N + FSize},
            read_size_chunk(Conn2, Size - N, Sep, [Data | Parts]);
        N ->
            {Part, MoreData} = split_binary(Data, Size),
            {[Part | Parts], Conn#conn{buf = [MoreData | Rest],
                                       frame_size = N + FSize}}
    end.

peek_byte(Conn) ->
    Conn2 = Conn#conn{buf = read_buffer(Conn)},
    Byte = binary:first(hd(Conn2#conn.buf)),
    {<<Byte>>, Conn2}.

eat_byte(Conn = #conn{buf = []}) ->
    eat_byte(Conn#conn{buf = read_buffer(Conn)});
eat_byte(Conn = #conn{buf = [Data | Rest]}) when size(Data) == 1 ->
    Conn#conn{buf = Rest};
eat_byte(Conn = #conn{buf = [Data | Rest]}) ->
    Conn#conn{buf = [binary:part(Data, 1, size(Data) - 1) | Rest]}.

read_buffer(Conn) ->
    read_buffer(Conn, 0).

read_buffer(Conn = #conn{buf = []}, RecvLen) ->
    case gen_tcpd:recv(Conn#conn.sock, RecvLen) of
        {ok, Packet} ->
            Data = [Packet];
        {error, closed} ->
            Data = none,
            throw(tcp_closed);
        {error, _} ->
            Data = none,
            throw(tcp_error)
    end,
    lager:debug("~B bytes received from ~s", [size(hd(Data)), Conn#conn.peer]),
    Data;
read_buffer(Conn, _) ->
    Conn#conn.buf.

log_frame(Frame, Peer) ->
    Cmd = string:to_upper(atom_to_list(Frame#frame.cmd)),
    lager:debug("Frame received from ~s~n\tCmd = ~s~n\tHeaders = ~p~n\tBody = ~p",
                [Peer, Cmd, Frame#frame.headers, Frame#frame.body]).

add_header(Frame, Key, Val) ->
    Frame#frame{headers = [{Key, Val} | Frame#frame.headers]}.

add_content_length(Frame) ->
    add_header(Frame, <<"content-length">>, iolist_size(Frame#frame.body)).

connect_frame() ->
    #frame{cmd = connect}.

connect_frame(Login, Pass) ->
    #frame{cmd = connect, headers = [{<<"login">>, Login}, {<<"passcode">>, Pass}]}.

connected_frame(Session) ->
    #frame{cmd = connected, headers = [{<<"session">>, Session}]}.

disconnect_frame() ->
    #frame{cmd = disconnect}.

message_frame(#frame{cmd = send, headers = Headers, body = Body}) ->
    add_header(#frame{cmd = message, headers = Headers, body = Body},
               <<"message-id">>, make_uuid_base64()).

message_frame(Dest, Body) ->
    #frame{cmd = message, headers = [{<<"message-id">>, make_uuid_base64()},
                                     {<<"destination">>, Dest}], body = Body}.

error_frame(Msg) ->
    #frame{cmd = error, headers = [{<<"message">>, Msg}]}.

error_frame(Msg, Body) ->
    add_content_length(
        #frame{cmd = error, headers = [{<<"message">>, Msg}], body = Body}).

get_header(Frame, Key) when is_list(Key) ->
    get_header(Frame, list_to_binary(Key));
get_header(Frame, Key) ->
    proplists:get_value(Key, Frame#frame.headers).

serialize_frame(Frame) ->
    #frame{cmd = Cmd0, headers = Headers, body = Body} = Frame,
    Cmd = string:to_upper(atom_to_list(Cmd0)),
    [Cmd, "\n", prepare_headers(Headers), "\n", Body, "\0"].

save_startup_timestamp() ->
    application:set_env(mumq, startup_timestamp, now()).

make_uuid() ->
    case get(startup_timestamp) of
        undefined ->
            {ok, Timestamp} = application:get_env(startup_timestamp),
            put(startup_timestamp, Timestamp);
        Timestamp ->
            true
    end,
    {make_ref(), Timestamp, node()}.

make_uuid_base64() ->
    base64:encode(erlang:md5(term_to_binary(make_uuid()))).
