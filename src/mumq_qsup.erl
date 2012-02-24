-module(mumq_qsup).

-behaviour(supervisor).

-export([start_link/0,
         start_queue/0]).

-export([init/1]).

-include("mumq.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_queue() ->
    supervisor:start_child(?MODULE, []).

init(_Args) ->
    {ok, {{simple_one_for_one, 5, 10},
          [?CHILD(mumq_queue, mumq_queue, worker, [])]}}.
