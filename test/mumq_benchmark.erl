-module(mumq_benchmark).

-compile(export_all).

start() ->
    ssl:start(),
    {ok, Conn} = mumq_client:connect_ssl("localhost"),

    io:format("Connected!\n"),


    %send_frame(Conn, Frame) ->
    %recv_frame(Conn) ->
    %recv_frame(Conn, Handlers) ->

    mumq_client:close(Conn).
