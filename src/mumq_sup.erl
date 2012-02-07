-module(mumq_sup).

-behaviour(supervisor).

-export([start_link/0,
         init/1]).

-include("mumq.hrl").

-define(CHILD(Id, Type, Args), {Id, {Id, start_link, Args},
                                permanent, 5000, Type, [Id]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {{one_for_one, 5, 10}, [?CHILD(mumq_tcpd, worker, [tcp, ?TCP_PORT]),
                                 ?CHILD(mumq_tcpd, worker, [ssl, ?TCP_PORT + 1])]}}.
