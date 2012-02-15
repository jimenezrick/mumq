-module(mumq_subs).

-export([start_link/0,
         init/0]).

-export([link/0,
         add_subscription/2,
         del_subscription/2,
         get_subscriptions/1]).

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
            ets:new(rev_ets_name(), ?ETS_OPTS),
            proc_lib:init_ack({ok, self()}),
            trap_exits_loop()
    catch
        error:badarg ->
            proc_lib:init_ack({error, {already_started, whereis(?MODULE)}})
    end.

rev_ets_name() ->
    list_to_atom(?MODULE_STRING ++ "_rev").

trap_exits_loop() ->
    receive
        {'EXIT', _, shutdown} ->
            exit(shutdown);
        {'EXIT', Pid, _} ->
            clean_subscriptions(Pid),
            trap_exits_loop()
    end.

link() ->
    link(whereis(?MODULE)),
    put('$ancestors', [?MODULE | get('$ancestors')]).

add_subscription(Queue, DeliveryProc) ->
    ets:insert(?MODULE, {Queue, DeliveryProc}),
    ets:insert(rev_ets_name(), {self(), DeliveryProc, Queue}).

del_subscription(Queue, DeliveryProc) ->
    ets:delete_object(?MODULE, {Queue, DeliveryProc}),
    ets:delete_object(rev_ets_name(), {self(), DeliveryProc, Queue}).

clean_subscriptions(Pid) ->
    Subs = ets:lookup(rev_ets_name(), Pid),
    ets:delete(rev_ets_name(), Pid),
    lists:foreach(fun({_, D, Q}) -> ets:delete_object(?MODULE, {Q, D}) end, Subs).

%% TODO: Buscar recursivamente y sacar el Pid de los procesos de las colas padre?
get_subscriptions(Queue) ->
    ets_lookup_element(?MODULE, Queue, 2).

ets_lookup_element(Tab, Key, Pos) ->
    try
        ets:lookup_element(Tab, Key, Pos)
    catch
        error:badarg ->
            []
    end.

%get_all_subqueues(Queue) ->
%Parts = binary:split(Queue, <<$/>>, [trim, global]),
