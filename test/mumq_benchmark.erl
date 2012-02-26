-module(mumq_benchmark).

-export([start/1,
         start_subscriber/4]).

-define(N_MSGS,   10000).
-define(MSG_SIZE, 100).

start([Host]) ->
    start([Host, "1"]);
start([Host, NumQueues]) ->
    Queues = queue_names(list_to_integer(NumQueues)),
    {ok, Conn} = mumq_client:connect(Host),
    io:format("Connected to ~s~n", [Host]),
    lists:foreach(
        fun(Q) ->
                spawn_link(?MODULE, start_subscriber, [self(), Host, Q, ?N_MSGS])
        end, Queues),
    Msg = random_payload(?MSG_SIZE),
    wait(ready, length(Queues)),
    Start = now(),
    send_messages(Conn, Queues, Msg, ?N_MSGS),
    io:format("Finished sending messages~n"),
    wait(finish, length(Queues)),
    Finish = now(),
    ok = mumq_client:disconnect(Conn),
    show_statistics(Start, Finish, length(Queues)).

queue_names(N) ->
    ["/" ++ integer_to_list(Q) || Q <- lists:seq(1, N)].

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
    send_messages(Conn, Queues, Msg, N - 1).

recv_messages(Conn, 0) ->
    Conn;
recv_messages(Conn, N) ->
    {ok, Frame, Conn2} = mumq_client:recv(Conn),
    message = mumq_client:frame_command(Frame),
    recv_messages(Conn2, N - 1).

wait(_, 0) ->
    ok;
wait(Msg, N) ->
    receive
        Msg ->
            wait(Msg, N - 1)
    end.

show_statistics(Start, Finish, NumQueues) ->
    Seconds = timer:now_diff(Finish, Start) / timer:seconds(1000),
    Messages = ?N_MSGS * NumQueues,
    io:format("~B messages delivered in ~f seconds~n",
              [Messages, Seconds]),
    io:format("~f messages/sec~n", [Messages / Seconds]).
