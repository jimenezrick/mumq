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
    ets:new(?MODULE, ?ETS_OPTS),
    ets:new(rev_ets_name(), ?ETS_OPTS),
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

rev_ets_name() ->
    list_to_atom(?MODULE_STRING ++ "_rev").

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

get_subscriptions(Queue) ->
    Subs = [ets_lookup_element(?MODULE, Q, 2) || Q <- generate_all_subqueues(Queue)],
    lists:append(Subs).

ets_lookup_element(Tab, Key, Pos) ->
    try
        ets:lookup_element(Tab, Key, Pos)
    catch
        error:badarg ->
            []
    end.

generate_all_subqueues(<<$/>>) ->
    [<<$/>>];
generate_all_subqueues(Queue) ->
    [{0, _} | Matches] = binary:matches(Queue, <<$/>>),
    generate_all_subqueues(Queue, Matches).

generate_all_subqueues(Queue, Matches) ->
    [<<$/>>, Queue | [binary:part(Queue, 0, Pos) || {Pos, _} <- Matches]].
