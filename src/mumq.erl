-module(mumq).

-export([start/0,
         stop/0]).

start() ->
    ssl:start(),
    application:start(gen_tcpd),
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).
