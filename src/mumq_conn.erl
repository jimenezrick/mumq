-module(mumq_conn).

-export([handle_connection/2]).

-record(state, {sock, recv_len, buf = []}).

%%% TODO: Sacar a otro modulo para hacer la parte cliente. Se podria implementar envio en streaming
%%%       tanto en el cliente como el servidor.
%%% TODO: Implementar las transacciones como un envio de un grupo de frames al destinatario?
%%%
%%% TODO: Handle exceptions: tcp_closed, tcp_error and stomp_error

handle_connection(Socket, none) ->
    {ok, [{recbuf, RecvLen}]} = gen_tcpd:getopts(Socket, [recbuf]),
    handle_connection(Socket, #state{sock = Socket, recv_len = RecvLen});
handle_connection(Socket, State) ->
    gen_tcpd:send(Socket, "HELO\n"),
    {Frame, State2} = read_frame(State),
    log_frame(Frame),
    handle_connection(Socket, State2).

log_frame({frame, Cmd, Headers, Body}) ->
    lager:debug("Frame:~n\tCmd = ~s~n\tHeaders = ~p~n\tBody = ~p",
                [Cmd, Headers, Body]).

read_frame(State) ->
    State2 = eat_empty_lines(State),
    {Cmd, State3} = read_line(State2),
    {Headers, State4} = read_headers(State3),
    BodySize = proplists:get_value("content-length", Headers, undefined),
    {Body, State5} = read_body(State4, BodySize),
    {{frame, Cmd, Headers, Body}, State5}.

read_headers(State) ->
    {Headers, State2} = read_headers(State, []),
    {parse_headers(Headers), State2}.

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

parse_headers(Headers) ->
    lists:map(fun parse_header/1, Headers).

parse_header({"content-length", Len}) ->
    {"content-length", list_to_integer(Len)};
parse_header(Header) ->
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
    case binary:split(Data, Sep) of
        [Part] ->
            read_chunk(State#state{buf = Rest}, Sep, [Part | Parts]);
        [<<>>, <<>>] ->
            {[<<>> | Parts], State#state{buf = Rest}};
        [<<>>, MoreData] ->
            {[<<>> | Parts], State#state{buf = [MoreData | Rest]}};
        [Part, <<>>] ->
            {[Part | Parts], State#state{buf = Rest}};
        [Part, MoreData] ->
            {[Part | Parts], State#state{buf = [MoreData | Rest]}}
    end.

read_size_chunk(State, Size, Sep) ->
    {Parts, State2} = read_size_chunk(State, Size, Sep, []),
    case peek_byte(State2) of
        {Sep, State3} ->
            {lists:reverse(Parts), eat_byte(State3)};
        _ ->
            throw(stomp_error)
    end.

read_size_chunk(State, 0, _, Parts) ->
    {Parts, State};
read_size_chunk(State, Size, Sep, Parts) ->
    [Data | Rest] = read_buffer(State, State#state.recv_len),
    case size(Data) of
        N when N =< Size ->
            State2 = State#state{buf = Rest},
            read_size_chunk(State2, Size - N, Sep, [Data | Parts]);
        _ ->
            {Part, MoreData} = split_binary(Data, Size),
            {[Part | Parts], State#state{buf = [MoreData | Rest]}}
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
    lager:debug("Read ~B bytes from socket", [size(Data)]),
    Data;
read_buffer(State, _) ->
    State#state.buf.
