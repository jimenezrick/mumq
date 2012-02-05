-module(mumq).

-export([start/0,
         stop/0]).

start() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(lager),
    application:start(gen_tcpd),
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).
