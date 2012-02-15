-module(mumq_subs).

-export([start_link/0,
         init/0]).

-export([link/0,
         add_subscriber/2,
         del_subscriber/2,
         get_subscribers/1]).

-define(ETS_OPTS, [duplicate_bag,
                   named_table,
                   public,
                   {read_concurrency, true}]).

start_link() ->
    proc_lib:start_link(?MODULE, init, []).

init() ->
    try register(?MODULE, self()) of
        true ->
            process_flag(trap_exit, true),
            ets:new(?MODULE, ?ETS_OPTS),
            proc_lib:init_ack({ok, self()}),
            supervise_loop()
    catch
        error:badarg ->
            proc_lib:init_ack({error, {already_started, whereis(?MODULE)}})
    end.

supervise_loop() ->
    receive
        {'EXIT', _From, shutdown} ->
            exit(shutdown);
        _ ->
            % XXX XXX XXX
            supervise_loop()
            % XXX XXX XXX
    end.

link() ->
    link(whereis(?MODULE)),
    put('$ancestors', [?MODULE | get('$ancestors')]).

add_subscriber(Queue, Pid) ->
    ets:insert(?MODULE, {Queue, Pid}).

del_subscriber(Queue, Pid) ->
    ets:delete_object(?MODULE, {Queue, Pid}).

%%%
%%% FIXME: Queda ver como gestionar los unlink(), no quitar los links?
%%%
%%% FIXME: Cuando un proceso conn muere, desuscribirlo automaticamente de todo
%%% TODO: Hacer una segunda tabla ETS mumq_subs_rev en la que se anote que subscripciones tiene un proceso?
%%%       Usarla para las limpiezas
%%%

get_subscribers(Queue) ->
    % TODO: Buscar recursivamente y sacar el Pid de los procesos de las colas padre?
    try
        ets:lookup_element(?MODULE, Queue, 2)
    catch
        error:badarg ->
            []
    end.
