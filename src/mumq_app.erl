-module(mumq_app).

-behaviour(application).

-export([start/0,
         start/2,
         stop/1]).

start() ->
    application:start(?MODULE).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    mumq_sup:start_link().

stop(_State) ->
    ok.
