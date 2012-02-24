-module(mumq_qsup).

-behaviour(supervisor).

-export([start_link/0,
         start_child/0,
         terminate_child/1]).

-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     temporary, 5000, Type, [Mod]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child() ->
    supervisor:start_child(?MODULE, []).

terminate_child(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

init(_Args) ->
    {ok, {{simple_one_for_one, 5, 10},
          [?CHILD(mumq_queue, mumq_queue, worker, [])]}}.
