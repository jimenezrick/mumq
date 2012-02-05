-module(mumq_conn).

-export([handle_connection/2]).

-record(state, {sock, buf = []}).

%%% TODO: Implementar las transacciones como un envio de un grupo de frames al destinatario?
%%% TODO: Implementar el leer tantos bytes cuando tengamos un Content-Length
%%% TODO: Mover el codigo de parseo a otro modulo para luego hacer una parte cliente

handle_connection(Socket, none) ->
    handle_connection(Socket, #state{sock = Socket});
handle_connection(Socket, State) ->
    gen_tcpd:send(Socket, "HELO\n"),
    {Frame, State2} = read_frame(State),
    log_frame(Frame),
    handle_connection(Socket, State2).

log_frame({frame, Cmd, Headers, Body}) ->
    lager:debug("Frame:~n\tCmd = ~s~n\tHeaders = ~p~n\tBody = ~p",
                [Cmd, Headers, Body]).

read_frame(State) ->
    State2 = consume_empty_lines(State),
    {Cmd, State3} = read_line(State2),
    {Headers, State4} = read_headers(State3),
    {Body, State5} = read_body(State4),
    {{frame, Cmd, Headers, Body}, State5}.

read_headers(State) ->
    read_headers(State, []).

read_headers(State, Headers) ->
    case read_line(State) of
        {<<>>, State2} ->
            {lists:reverse(Headers), State2};
        {Line, State2} ->
            [Key, Val] = lists:map(fun strip_spaces/1, binary:split(Line, <<":">>)),
            read_headers(State2, [{Key, Val} | Headers])
    end.

strip_spaces(Bin) ->
    binary:replace(Bin, <<$ >>, <<>>, [global]).

read_line(State) ->
    Concat = fun(B1, B2) -> <<B2/binary, B1/binary>> end,
    {Chunk, State2} = read_chunk(State, <<$\n>>),
    {lists:foldl(Concat, <<>>, Chunk), State2}.

read_body(State) ->
    %read_chunk(State, <<$\0>>). % FIXME FIXME FIXME
    read_chunk(State, <<"x">>).

consume_empty_lines(State) ->
    case read_line(State) of
        {<<>>, State2} ->
            consume_empty_lines(State2);
        {Line, State2} ->
            Buf = State2#state.buf,
            State2#state{buf = [<<Line/binary, "\n">> | Buf]}
    end.

% TODO TODO TODO
%read_chunk(State, Size) ->
%read_chunk(State, Size) ->
%read_chunk(State, Size) ->
% TODO TODO TODO

read_chunk(State, Sep) ->
    {Parts, State2} = read_chunk(State, Sep, []),
    {lists:reverse(Parts), State2}.

read_chunk(State = #state{buf = []}, Sep, Parts) ->
    % TODO: handle error, throw
    {ok, Data} = gen_tcpd:recv(State#state.sock, 0),
    lager:debug("Read ~B bytes from socket", [size(Data)]),
    read_chunk(State#state{buf = [Data]}, Sep, Parts);
read_chunk(State, Sep, Parts) ->
    [Data | Rest] = State#state.buf,
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
