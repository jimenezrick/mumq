-module(mumq_qsup).

-behaviour(supervisor).

-export([start_link/0,
         start_queue/0]).

-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 0, Type, [Mod]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_queue() ->
    supervisor:start_child(?MODULE, []).

init(_Args) ->
    {ok, {{simple_one_for_one, 5, 10},
          [?CHILD(mumq_queue, mumq_queue, worker, [])]}}.
