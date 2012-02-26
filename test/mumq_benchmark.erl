-module(mumq_benchmark).

-export([start/1,
         start_subscriber/4]).

-define(N_MSGS,   1000).
-define(MSG_SIZE, 1024).

start([Host]) ->
    {ok, Conn} = mumq_client:connect(Host),
    io:format("Connected to ~s~n", [Host]),
    spawn_link(?MODULE, start_subscriber, [self(), Host, "/foo", ?N_MSGS]),
    spawn_link(?MODULE, start_subscriber, [self(), Host, "/bar", ?N_MSGS]),
    Msg = random_payload(?MSG_SIZE),
    ok = wait(ready, 2),
    ok = wait(ready, 2),
    send_messages(Conn, ["/foo", "/bar"], Msg, ?N_MSGS),
    io:format("Finished sending messages~n"),
    ok = wait(finish, 10),
    ok = wait(finish, 10),
    ok = mumq_client:disconnect(Conn).

start_subscriber(Parent, Host, Queue, N) ->
    {ok, Conn} = mumq_client:connect(Host),
    mumq_client:subscribe(Conn, Queue),
    io:format("Subscriber ~s ready~n", [Queue]),
    Parent ! ready,
    Conn2 = recv_messages(Conn, N),
    Parent ! finish,
    io:format("Subscriber ~s finished receiving messages~n", [Queue]),
    ok = mumq_client:disconnect(Conn2).

random_payload(Size) ->
    crypto:start(),
    crypto:rand_bytes(Size).

send_messages(_, _, _, 0) ->
    ok;
send_messages(Conn, Queues, Msg, N) ->
    Fun = fun(Q) -> mumq_client:send(Conn, Q, Msg) end,
    lists:foreach(Fun, Queues),
    send_messages(Conn, Queues, Msg, N - length(Queues)).

recv_messages(Conn, 0) ->
    Conn;
recv_messages(Conn, N) ->
    {ok, Frame, Conn2} = mumq_client:recv(Conn),
    message = mumq_client:frame_command(Frame),
    recv_messages(Conn2, N - 1).

wait(Msg, Timeout) ->
    receive
        Msg ->
            ok
    after
        timer:seconds(Timeout) ->
            {error, timeout}
    end.
