-module(mumq_app).

-behaviour(application).

-export([start/2,
         stop/1]).

-include("mumq.hrl").

-ifdef(DEBUG).
start(_StartType, _StartArgs) ->
    lager:set_loglevel(lager_console_backend, debug),
    mumq_sup:start_link().
-else.
start(_StartType, _StartArgs) ->
    mumq_sup:start_link().
-endif.

stop(_State) ->
    ok.
