-module(mumq_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-include("mumq.hrl").

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    case
        {application:get_env(ssl_certfile), application:get_env(ssl_keyfile)}
    of
        {{ok, _}, {ok, _}} ->
            SslChild = [?CHILD(mumq_ssld, mumq_tcpd, worker,
                               [mumq_ssld, ssl, ?STOMP_TCP_PORT + 1])];
        _ ->
            SslChild = []
    end,
    case application:get_env(enable_irc) of
        {ok, true} ->
            IrcChild = [?CHILD(mumq_ircd, mumq_ircd, worker, [])];
        undefined ->
            IrcChild = []
    end,
    {ok, {{one_for_one, 5, 10}, [?CHILD(mumq_pers, mumq_pers, worker, []),
                                 ?CHILD(mumq_qsup, mumq_qsup, supervisor, []),
                                 ?CHILD(mumq_tcpd, mumq_tcpd, worker,
                                        [mumq_tcpd, tcp, ?STOMP_TCP_PORT])] ++ SslChild ++ IrcChild}}.
