-module(mumq_benchmark).

-compile(export_all).

start() ->
    {ok, Conn} = mumq_client:connect("localhost"),

    io:format("Connected!\n"),

    %
    % FIXME: No funciona con SSL
    %

    %send_frame(Conn, Frame) ->
    %recv_frame(Conn) ->
    %recv_frame(Conn, Handlers) ->

    mumq_client:close(Conn).