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
         del_subscription/2,
         get_subscriptions/1]).

-define(ETS_OPTS, [duplicate_bag,
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
    ets:insert(mumq_subs, {Queue, DeliveryProc}),
    ets:insert(mumq_subs_rev, {self(), DeliveryProc, Queue}).

del_subscription(Queue, DeliveryProc) ->
    ets:delete_object(mumq_subs, {Queue, DeliveryProc}),
    ets:delete_object(mumq_subs_rev, {self(), DeliveryProc, Queue}).

clean_subscriptions(Pid) ->
    Subs = ets:lookup(mumq_subs_rev, Pid),
    ets:delete(mumq_subs_rev, Pid),
    lists:foreach(fun({_, D, Q}) -> ets:delete_object(mumq_subs, {Q, D}) end, Subs).

get_subscriptions(Queue) ->
    Subs = [ets_lookup_element(mumq_subs, Q, 2) || Q <- generate_all_subqueues(Queue)],
    lists:append(Subs).

ets_lookup_element(Tab, Key, Pos) ->
    try
        ets:lookup_element(Tab, Key, Pos)
    catch
        error:badarg ->
            []
    end.

generate_all_subqueues(Queue) ->
    [{0, _} | Matches] = binary:matches(Queue, <<$/>>),
    [Queue | generate_all_subqueues(Queue, Matches)].

generate_all_subqueues(Queue, Matches) ->
    [binary:part(Queue, 0, Pos) || {Pos, _} <- Matches].
