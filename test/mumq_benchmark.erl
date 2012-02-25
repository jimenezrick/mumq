-module(mumq_benchmark).

-compile(export_all).

start() ->
    {ok, Conn} = mumq_client:connect("localhost"),

    %send_frame(Conn, Frame) ->
    %recv_frame(Conn) ->
    %recv_frame(Conn, Handlers) ->

    mumq_client:close(Conn).
