-module(mumq_conn).

-export([handle_connection/2]).

-record(state, {sock, buf = []}).

%%% TODO: Mirar cuandos bytes leemos de media en cada recepcion
%%% TODO: Implementar las transacciones como un envio de un grupo de frames al destinatario?

handle_connection(Socket, State) ->
    gen_tcpd:send(Socket, "HELO\n"),
    Frame = read_frame(#state{sock = Socket}),
    print_frame(Frame),
    handle_connection(Socket, State).

print_frame({frame, Cmd, Headers, Body}) ->
    % TODO: Usar lager!
    io:format("--------------------------------------------------~n"),
    io:format("Cmd = ~s~n", [Cmd]),
    io:format("Headers = ~p~n", [Headers]),
    io:format("Body = ~p~n", [Body]),
    io:format("--------------------------------------------------~n").

read_frame(State) ->
    {Cmd, State2} = read_line(State),
    {Headers, State3} = read_headers(State2),
    {Body, State4} = read_body(State3),
    {{frame, Cmd, Headers, Body}, consume_empty_lines(State4)}.

read_headers(State) ->
    read_headers(State, []).

read_headers(State, Headers) ->
    case read_line(State) of
        {<<>>, State2} ->
            {lists:reverse(Headers), State2};
        {Line, State2} ->
            [Key, Val] = binary:split(Line, <<":">>),
            read_headers(State2, [{Key, Val} | Headers])
    end.

read_line(State) ->
    Concat = fun(B1, B2) -> <<B2/binary, B1/binary>> end,
    {Chunk, State2} = read_chunk(State, <<$\n>>, []),
    {lists:foldl(Concat, <<>>, Chunk), State2}.

read_body(State) ->
    read_chunk(State, <<$\0>>, []).

consume_empty_lines(State) ->
    case read_line(State) of
        {<<>>, State2} ->
            consume_empty_lines(State2);
        {Line, State2} ->
            Buf = State2#state.buf,
            State2#state{buf = [<<Line/binary, "\n">> | Buf]}
    end.

read_chunk(State = #state{buf = []}, Sep, Parts) ->
    % TODO: handle error, throw
    {ok, Data} = gen_tcpd:recv(State#state.sock, 0),
    read_chunk(State#state{buf = [Data]}, Sep, Parts);
read_chunk(State, Sep, Parts) ->
    [Data | Rest] = State#state.buf,
    case binary:split(Data, Sep) of
        [<<>>] ->
            {<<>>, State#state{buf = Rest}};
        [Part] ->
            read_chunk(State#state{buf = Rest}, Sep, [Part | Parts]);
        [<<>>, MoreData] ->
            {<<>>, State#state{buf = [MoreData | Rest]}};
        [Part, <<>>] ->
            {lists:reverse([Part | Parts]), State#state{buf = Rest}};
        [Part, MoreData] ->
            {lists:reverse([Part | Parts]), State#state{buf = [MoreData | Rest]}}
    end.
