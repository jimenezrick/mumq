-module(mumq).

-export([start/0,
         stop/0,
         subscribed_clients/0,
         registered_queues/0,
         queue_info/1]).

start() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(lager),
    application:start(gen_tcpd),
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

subscribed_clients() ->
    case mumq_subs:subscribed_clients() of
        [] ->
            ok;
        Subs ->
            io:format("~-10s~-20s~s~n", ["Pid", "Queue", "Id"]),
            lists:foreach(
                fun({P, Q, I}) ->
                        io:format("~-10w~-20s~s~n", [P, Q, I])
                end, Subs)
    end.

registered_queues() ->
    case mumq_pers:registered_queues() of
        [] ->
            ok;
        Queues ->
            io:format("~-10s~s~n", ["Pid", "Queue"]),
            lists:foreach(
                fun({P, Q}) ->
                        io:format("~-10w~s~n", [P, Q])
                end, Queues)
    end.

queue_info(Queue) ->
    {Frames, Subs} = mumq_pers:queue_info(list_to_binary(Queue)),
    case Frames of
        [] ->
            true;
        _ ->
            lists:foreach(
                fun({S, F}) ->
                        io:format("=== FRAME ~B =======================~n", [S]),
                        io:format("~s~n", [mumq_stomp:serialize_frame_no_null(F)])
                end, Frames),
            io:format("====================================~n")
    end,
    case Subs of
        [] ->
            ok;
        _ ->
            io:format("~-10s~s~n", ["Id", "AckSeq"]),
            lists:foreach(
                fun({I, S}) ->
                        io:format("~-10s~B~n", [I, S])
                end, Subs)
    end.
