-module(mumq_stomp).

-export([write_frame/2]).

write_frame(Socket, Frame) ->
    {frame, Cmd, Headers, Body} = Frame,
    Data = [Cmd, $\n, prepare_headers(Headers), $\n, Body, $\0],
    gen_tcpd:send(Socket, Data).

prepare_headers(Headers) ->
    [prepare_header(H) || H <- Headers].

prepare_header({"content-length", Len}) when is_list(Len) ->
    prepare_header({"content-length", list_to_integer(Len)});
prepare_header({Key, Val}) ->
    [Key, $:, Val, $\n].
