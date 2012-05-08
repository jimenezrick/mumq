-module(mumq_util).

-export([format_peername/1]).

format_peername(Socket) ->
    {ok, {{O1, O2, O3, O4}, P}} = gen_tcpd:peername(Socket),
    io_lib:format("~B.~B.~B.~B:~B", [O1, O2, O3, O4, P]).
