-module(mumq).

-export([start/0,
         stop/0,
         connected_clients/0]).

start() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(lager),
    application:start(gen_tcpd),
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

connected_clients() ->
    io:format("---------------------------------------------------------------------------------~n"),
    inet:i(),
    io:format("---------------------------------------------------------------------------------~n"),
    io:format("~-16s~-40s~s~n", ["Pid", "Queue", "Id"]),
    lists:foreach(
        fun({P, Q, I}) ->
                io:format("~-16w~-40s~s~n", [P, Q, I])
        end, mumq_subs:subscribed_clients()),
    io:format("---------------------------------------------------------------------------------~n").
