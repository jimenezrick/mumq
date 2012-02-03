-module(mumq_app).

-behaviour(application).

-export([start/2,
         stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    error_logger:info_report([{"muMQ", "application started"}]),
    mumq_sup:start_link().

stop(_State) ->
    error_logger:info_report([{"muMQ", "application stopped"}]).
