-module(mumq_ircd).

-behaviour(gen_tcpd).

-export([start_link/0,
         handle_connection/1]).

-export([init/1,
         handle_connection/2,
         handle_info/2,
         terminate/2]).

-include("mumq.hrl").

-define(IRC_TCP_OPTS, [{active, once},
                       {packet, line},
                       {nodelay, true},
                       {keepalive, true}]).

-record(state, {servname = net_adm:localhost(),
                sock,
                nick}).



%%%
%%% TODO: Usar allow_users de la app config, igual que en STOMP para autenticar usuarios
%%%       Hacer una funcion auxiliar comun para STOMP y IRC
%%% TODO: Actualizar README
%%%
%%%  Los canales son solo de subscripcion, no se puede publicar
%%%

%%% FIXME: Como debe presentarse el servidor? Para que lo pille el cliente de IRC?
%%% FIXME: Como se reciben los mensajes publicos?
%%%
%%% XXX: Los cliente se subscriben con JOIN y se desubscriben al salir o al morir

%%% Strip \r from line
%%% NICK name
%%% USER username ...
%%% JOIN #channel
%%% PART #channel ...
%%% QUIT ...
%%%




%%% Secuencia de logeo:
%%% Opcional: PASS <pass>
%%%           NICK <nick>
%%%           USER <user> ...

start_link() ->
    gen_tcpd:start_link({local, ?MODULE}, ?MODULE, [], tcp, ?IRC_TCP_PORT,
                        [{acceptors, ?TCP_ACCEPTORS},
                         {socket_options, ?IRC_TCP_OPTS ++ [{reuseaddr, true}]}]).

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.



handle_connection(Socket, State) ->
    lager:info("*** IRC client connected"), % XXX

    State2 = State#state{sock = Socket, nick = "fuu"}, % XXX

    reply_welcome(State2),
    handle_connection(State2).




handle_connection(State = #state{sock = Socket}) ->
    receive
        {tcp, Socket, Line} ->
            io:format("Line: ~p~n", [Line]),
            gen_tcpd:setopts(Socket, [{active, once}])
        %{tcp_closed, Socket} ->
        %{tcp_error, Socket, Reason} ->
    end,
    handle_connection(State).

handle_info({'EXIT', _Pid, _}, _State) ->
    noreply.

terminate(_Reason, _State) ->
    ok.







reply_welcome(State) ->
    {ok, Version} = application:get_key(vsn),
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_local_time(
            mumq_stomp:load_startup_timestamp()),
    StartupDate = io_lib:format("~B/~B/~B ~B:~2..0B:~2..0B", [Year, Month, Day, Hour, Min, Sec]),
    reply_code(State, 1, ":Welcome to the Internet Relay Network ~s", [State#state.nick]),
    reply_code(State, 2, ":Your host is ~s, running version muMQ-~s", [State#state.servname, Version]),
    reply_code(State, 3, ":This server was created ~s", [StartupDate]),
    reply_code(State, 4, "~s muMQ-~s", [State#state.servname, Version]).

reply_code(State, Code, Format, Data) ->
    Line = io_lib:format(":~s ~3..0B ~s " ++ Format ++ "\r\n",
                         [State#state.servname, Code, State#state.nick] ++ Data),
    gen_tcpd:send(State#state.sock, Line).
