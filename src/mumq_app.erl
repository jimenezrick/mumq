-module(mumq_app).

-behaviour(application).

-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->
    error_logger:info_report([{"muMQ", "application started"}]),
    mumq_subs:init(),
    mumq_sup:start_link().

stop(_State) ->
    mumq_subs:terminate(),
    error_logger:info_report([{"muMQ", "application stopped"}]).
