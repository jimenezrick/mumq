-module(mumq_app).

-behaviour(application).

-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->
    % XXX
    mumq_queue:start_link(),
    % XXX
    mumq_sup:start_link().

stop(_State) ->
    ok.
