-module(mumq_subs).

-behaviour(gen_server).

-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([link/0,
         add_subscription/2,
         add_subscription/3,
         del_subscription/2,
         del_subscription/3,
         get_subscriptions/1]).

-define(ETS_OPTS, [bag,
                   named_table,
                   public,
                   {read_concurrency, true}]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, self(), []).

init(SupPid) ->
    process_flag(trap_exit, true),
    ets:new(mumq_subs, ?ETS_OPTS),
    ets:new(mumq_subs_rev, ?ETS_OPTS),
    {ok, SupPid}.

handle_call(_Req, _From, _State) ->
    exit(not_implemented).

handle_cast(_Req, _State) ->
    exit(not_implemented).

handle_info({'EXIT', SupPid, Reason}, SupPid) ->
    exit(Reason);
handle_info({'EXIT', Pid, _Reason}, SupPid) ->
    clean_subscriptions(Pid),
    {noreply, SupPid}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, _State, _Extra) ->
    exit(not_implemented).

link() ->
    link(whereis(?MODULE)),
    put('$ancestors', [?MODULE | get('$ancestors')]).

add_subscription(Queue, DeliveryProc) ->
    add_subscription(Queue, undefined, DeliveryProc).

add_subscription(Queue, Id, DeliveryProc) ->
    ets:insert(mumq_subs, {Queue, Id, DeliveryProc}),
    ets:insert(mumq_subs_rev, {self(), Queue, Id, DeliveryProc}).

del_subscription(Queue, DeliveryProc) ->
    del_subscription(Queue, undefined, DeliveryProc).

del_subscription(Queue, Id, DeliveryProc) ->
    ets:delete_object(mumq_subs, {Queue, Id, DeliveryProc}),
    ets:delete_object(mumq_subs_rev, {self(), Queue, Id, DeliveryProc}).

clean_subscriptions(Pid) ->
    Subs = ets:lookup(mumq_subs_rev, Pid),
    ets:delete(mumq_subs_rev, Pid),
    lists:foreach(fun({_, Q, I, D}) -> ets:delete_object(mumq_subs, {Q, I, D}) end, Subs).

get_subscriptions(Queue) ->
    Subs = [ets:lookup(mumq_subs, Q) || Q <- gen_queue_hierarchy(Queue)],
    [{I, D} || {_, I, D} <- lists:append(Subs)].

gen_queue_hierarchy(Queue) ->
    [{0, _} | Matches] = binary:matches(Queue, <<$/>>),
    [Queue | gen_queue_hierarchy(Queue, Matches)].

gen_queue_hierarchy(Queue, Matches) ->
    [binary:part(Queue, 0, Pos) || {Pos, _} <- Matches].
