-module(mumq_conn).

-export([handle_connection/2]).

-record(state, {sock,
                peer,
                recv_len,
                max_frame_size,
                frame_size = 0,
                buf = []}).

-define(MAX_FRAME_SIZE, 4 * 1024 * 1024).

%%%-----------------------------------------------------------------------------
%%% TODO: Sacar a otro modulo para hacer la parte cliente.
%%% TODO: Implementar las transacciones como un envio de un grupo de frames
%%%       al destinatario?
%%%-----------------------------------------------------------------------------

handle_connection(Socket, none) ->
    {ok, Peer0} = gen_tcpd:peername(Socket),
    Peer = format_peer(Peer0),
    lager:info("New connection from ~s", [Peer]),
    {ok, [{recbuf, RecvLen}]} = gen_tcpd:getopts(Socket, [recbuf]),
    Max = max_frame_size(),
    handle_connection(Socket, #state{sock = Socket, peer = Peer,
                                     recv_len = RecvLen, max_frame_size = Max});
handle_connection(Socket, State) ->
    try
        gen_tcpd:send(Socket, "HELO\n"),
        case read_frame(State) of
            {error, bad_frame} ->
                lager:info("Invalid frame received from ~s", [State#state.peer]),
                gen_tcpd:close(Socket);
            {error, bad_frame_size} ->
                lager:info("Frame too big received from ~s", [State#state.peer]),
                gen_tcpd:close(Socket);
            {Frame, State2} ->
                log_frame(Frame, State2#state.peer),
                handle_connection(Socket, State2)
        end
    catch
        throw:tcp_closed ->
            lager:info("Connection closed by ~s", [State#state.peer]);
        throw:tcp_error ->
            lager:info("Connection error with ~s", [State#state.peer])
    end.

format_peer({{O1, O2, O3, O4}, P}) ->
    io_lib:format("~B.~B.~B.~B:~B", [O1, O2, O3, O4, P]).

max_frame_size() ->
    case application:get_env(max_frame_size) of
        undefined ->
            ?MAX_FRAME_SIZE;
        {ok, Max} ->
            Max
    end.

log_frame({frame, Cmd, Headers, Body}, Peer) ->
    lager:debug("Frame received from ~s~n\tCmd = ~s~n\tHeaders = ~p~n\tBody = ~p",
                [Peer, Cmd, Headers, Body]).

read_frame(State) ->
    try
        read_frame2(State)
    catch
        throw:bad_frame ->
            {error, bad_frame};
        throw:bad_frame_size ->
            {error, bad_frame_size}
    end.

read_frame2(State) ->
    State2 = eat_empty_lines(State),
    {Cmd, State3} = read_line(State2),
    {Headers, State4} = read_headers(State3),
    BodySize = proplists:get_value("content-length", Headers, undefined),
    {Body, State5} = read_body(State4, BodySize),
    {{frame, Cmd, Headers, Body}, State5}.

read_headers(State) ->
    try
        {Headers, State2} = read_headers(State, []),
        {parse_headers(State2, Headers), State2}
    catch
        error:_ ->
            throw(bad_frame)
    end.

read_headers(State, Headers) ->
    case read_line(State) of
        {<<>>, State2} ->
            {lists:reverse(Headers), State2};
        {Line, State2} ->
            [Key0, Val0] = lists:map(fun strip_spaces/1, binary:split(Line, <<":">>)),
            Key = string:to_lower(binary_to_list(Key0)),
            Val = binary_to_list(Val0),
            read_headers(State2, [{Key, Val} | Headers])
    end.

strip_spaces(Bin) ->
    binary:replace(Bin, <<$ >>, <<>>, [global]).

parse_headers(State, Headers) ->
    [parse_header(State, H) || H <- Headers].

parse_header(State, {"content-length", StrLen}) ->
    case list_to_integer(StrLen) of
        Len when Len + State#state.frame_size > State#state.max_frame_size ->
            throw(bad_frame_size);
        Len ->
            true
    end,
    {"content-length", Len};
parse_header(_, Header) ->
    Header.

read_line(State) ->
    Concat = fun(B1, B2) -> <<B2/binary, B1/binary>> end,
    {Chunk, State2} = read_chunk(State, <<$\n>>),
    {lists:foldl(Concat, <<>>, Chunk), State2}.

read_body(State, undefined) ->
    read_chunk(State, <<$x>>); % FIXME $\0
read_body(State, Size) ->
    read_size_chunk(State, Size, <<$x>>). % FIXME $\0

eat_empty_lines(State) ->
    case peek_byte(State) of
        {<<$\n>>, State2} ->
            eat_empty_lines(eat_byte(State2));
        {_, State2} ->
            State2
    end.

read_chunk(State, Sep) ->
    {Parts, State2} = read_chunk(State, Sep, []),
    {lists:reverse(Parts), State2}.

read_chunk(State, Sep, Parts) ->
    [Data | Rest] = read_buffer(State),
    Size = State#state.frame_size,
    case binary:split(Data, Sep) of
        L when size(hd(L)) + Size > State#state.max_frame_size ->
            throw(bad_frame_size);
        [Part] ->
            read_chunk(State#state{buf = Rest, frame_size = Size + size(Part)},
                       Sep, [Part | Parts]);
        [<<>>, <<>>] ->
            {[<<>> | Parts], State#state{buf = Rest}};
        [<<>>, MoreData] ->
            {[<<>> | Parts], State#state{buf = [MoreData | Rest]}};
        [Part, <<>>] ->
            {[Part | Parts], State#state{buf = Rest, frame_size = Size + size(Part)}};
        [Part, MoreData] ->
            {[Part | Parts], State#state{buf = [MoreData | Rest],
                                         frame_size = Size + size(Part)}}
    end.

read_size_chunk(State, Size, Sep) ->
    {Parts, State2} = read_size_chunk(State, Size, Sep, []),
    case peek_byte(State2) of
        {Sep, State3} ->
            {lists:reverse(Parts), eat_byte(State3)};
        _ ->
            throw(bad_frame)
    end.

read_size_chunk(State, 0, _, Parts) ->
    {Parts, State};
read_size_chunk(State, Size, Sep, Parts) ->
    [Data | Rest] = read_buffer(State, min(Size, State#state.recv_len)),
    FSize = State#state.frame_size,
    case size(Data) of
        N when N =< Size ->
            State2 = State#state{buf = Rest, frame_size = N + FSize},
            read_size_chunk(State2, Size - N, Sep, [Data | Parts]);
        N ->
            {Part, MoreData} = split_binary(Data, Size),
            {[Part | Parts], State#state{buf = [MoreData | Rest],
                                         frame_size = N + FSize}}
    end.

peek_byte(State) ->
    State2 = State#state{buf = read_buffer(State)},
    Byte = binary:first(hd(State2#state.buf)),
    {<<Byte>>, State2}.

eat_byte(State = #state{buf = []}) ->
    eat_byte(State#state{buf = read_buffer(State)});
eat_byte(State = #state{buf = [Data | Rest]}) when size(Data) == 1 ->
    State#state{buf = Rest};
eat_byte(State = #state{buf = [Data | Rest]}) ->
    State#state{buf = [binary:part(Data, 1, size(Data) - 1) | Rest]}.

read_buffer(State) ->
    read_buffer(State, 0).

read_buffer(State = #state{buf = []}, RecvLen) ->
    case gen_tcpd:recv(State#state.sock, RecvLen) of
        {ok, Packet} ->
            Data = [Packet];
        {error, closed} ->
            Data = none,
            throw(tcp_closed);
        {error, _} ->
            Data = none,
            throw(tcp_error)
    end,
    lager:debug("~B bytes received from ~s", [size(hd(Data)), State#state.peer]),
    Data;
read_buffer(State, _) ->
    State#state.buf.
