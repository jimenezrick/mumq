-module(mumq_sup).

-behaviour(supervisor).

-export([start_link/0,
         init/1]).

-include("mumq.hrl").

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {{rest_for_one, 5, 10}, [?CHILD(mumq_subs, mumq_subs, worker, []),
                                  ?CHILD(mumq_tcpd, mumq_tcpd, worker,
                                         [mumq_tcpd, tcp, ?TCP_PORT]),
                                  ?CHILD(mumq_ssld, mumq_tcpd, worker,
                                         [mumq_ssld, ssl, ?TCP_PORT + 1])]}}.
