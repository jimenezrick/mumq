-module(mumq_ircd).

-behaviour(gen_tcpd).

-export([start_link/0]).

-export([init/1,
         handle_connection/2,
         handle_info/2,
         terminate/2]).

-include("mumq.hrl").

-define(IRC_TCP_OPTS, [{active, once},
                       {packet, line},
                       {nodelay, true},
                       {keepalive, true}]).

-record(state, {servname = net_adm:localhost(), nick, user}).



%%%
%%% TODO: Usar allow_users de la app config, igual que en STOMP para autenticar usuarios
%%%       Hacer una funcion auxiliar comun para STOMP y IRC
%%% TODO: Actualizar README
%%%
%%%  Los canales son solo de subscripcion, no se puede publicar
%%%

%%% FIXME: Como se reciben los mensajes publicos? PRIVMSG
%%%
%%% XXX: Los cliente se subscriben con JOIN y se desubscriben al salir o al morir

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






handle_connection(Socket, State = #state{nick = Nick, user = User}) when
        Nick == undefined; User == undefined ->
    case receive_message(Socket) of
        {'NICK', [Nick2]} ->
            State2 = State#state{nick = Nick2};
        {'USER', [User2 | _]} ->
            State2 = State#state{user = User2}
    end,
    lager:info("New IRC client from ~s", [gen_tcpd:peername(Socket)]),
    handle_connection(Socket, State2);
handle_connection(Socket, State) ->
    reply_welcome(Socket, State),
    handle_client(Socket, State).





handle_info({'EXIT', _Pid, _}, _State) ->
    noreply.

terminate(_Reason, _State) ->
    ok.















handle_client(Socket, State) ->
    case receive_message(Socket) of
        {'PING', Args} ->
            reply(Socket, State, 'PONG', Args)
    end,
    handle_client(Socket, State).











receive_message(Socket) ->
    SocketPort = gen_tcpd:sock(Socket),
    receive
        {tcp, SocketPort, Line} ->
            gen_tcpd:setopts(Socket, [{active, once}]),
            Line2 = strip_line(Line),
            lager:debug("IRC message from ~s: ~s", [gen_tcpd:peername(Socket), Line2]),
            parse_message(Line2);
        % XXX XXX XXX
        {tcp_closed, SocketPort} ->
            lager:info("*** IRC client disconnected");
        {tcp_error, SocketPort, Reason} ->
            lager:info("*** IRC client error")
            % XXX XXX XXX
    end.





strip_line(Line) ->
    Line2 = string:strip(Line, right, $\n),
    string:strip(Line2, right, $\r).

parse_message(Line) ->
    Tokens = string:tokens(Line, " "),
    {list_to_atom(hd(Tokens)), tl(Tokens)}.

reply_welcome(Socket, State) ->
    {ok, Version} = application:get_key(vsn),
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_local_time(
            mumq_stomp:load_startup_timestamp()),
    StartupDate = io_lib:format("~B/~B/~B ~B:~2..0B:~2..0B", [Year, Month, Day, Hour, Min, Sec]),
    reply_code(Socket, State, 1, ":Welcome to the Internet Relay Network ~s", [State#state.nick]),
    reply_code(Socket, State, 2, ":Your host is ~s, running version muMQ-~s", [State#state.servname, Version]),
    reply_code(Socket, State, 3, ":This server was created ~s", [StartupDate]),
    reply_code(Socket, State, 4, "~s muMQ-~s", [State#state.servname, Version]).

reply_code(Socket, State, Code, Format, Data) ->
    Line = io_lib:format(":~s ~3..0B ~s " ++ Format ++ "\r\n",
                         [State#state.servname, Code, State#state.nick] ++ Data),
    gen_tcpd:send(Socket, Line).

reply(Socket, State, Cmd, Args) ->
    Line = io_lib:format(":~s ~s ~s\r\n", [State#state.servname, atom_to_list(Cmd), string:join(Args, " ")]),
    gen_tcpd:send(Socket, Line).
