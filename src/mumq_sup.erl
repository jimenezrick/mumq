-module(mumq_sup).

-behaviour(supervisor).

-export([start_link/0,
         init/1]).

-define(CHILD(Id, Type), {Id, {Id, start_link, []}, permanent, 5000, Type, [Id]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {{one_for_one, 5, 10}, [?CHILD(mumq_tcpd, worker)]}}.
