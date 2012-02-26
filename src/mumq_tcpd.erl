-module(mumq_tcpd).

-behaviour(gen_tcpd).

-export([start_link/3]).

-export([init/1,
         handle_connection/2,
         handle_info/2,
         terminate/2]).

-include("mumq.hrl").

start_link(Name, Type, Port) ->
    case Type of
        ssl ->
            case
                {application:get_env(ssl_certfile),
                 application:get_env(ssl_keyfile)}
            of
                {{ok, CertFile}, {ok, KeyFile}} ->
                    SslOpts = [{certfile, CertFile}, {keyfile, KeyFile}];
                _ ->
                    SslOpts = []
            end;
        _ ->
            SslOpts = []
    end,
    gen_tcpd:start_link({local, Name}, ?MODULE, Name, Type, Port,
                        [{acceptors, ?TCP_ACCEPTORS},
                         {socket_options, ?TCP_OPTS ++ [{reuseaddr, true}] ++ SslOpts}]).

init(mumq_tcpd) ->
    process_flag(trap_exit, true),
    mumq_subs:create_table(),
    {ok, none};
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, none}.

handle_connection(Socket, _State) ->
    mumq_conn:handle_connection(Socket).

handle_info({'EXIT', Pid, _}, _State) ->
    mumq_subs:del_subscriptions(Pid),
    noreply.

terminate(_Reason, _State) ->
    ok.
