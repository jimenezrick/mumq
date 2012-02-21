-module(mumq_subs).

-behaviour(gen_server).

-export([start_link/0,
         link/0,
         add_subscription/3,
         del_subscription/3,
         get_subscriptions/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(ETS_OPTS, [bag,
                   named_table,
                   public,
                   {read_concurrency, true}]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, self(), []).

init(SupPid) ->
    process_flag(trap_exit, true),
    ets:new(?MODULE, ?ETS_OPTS),
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

add_subscription(Queue, Id, DeliveryProc) ->
    case ets:match_object(?MODULE, {self(), Queue, Id, DeliveryProc}) of
        [] ->
            ets:insert(?MODULE, {Queue, Id, DeliveryProc}),
            ets:insert(?MODULE, {self(), Queue, Id, DeliveryProc});
        [_] ->
            false
    end.

del_subscription(Queue, Id, DeliveryProc) ->
    case ets:match_object(?MODULE, {self(), Queue, Id, DeliveryProc}) of
        [_] ->
            ets:delete_object(?MODULE, {Queue, Id, DeliveryProc}),
            ets:delete_object(?MODULE, {self(), Queue, Id, DeliveryProc});
        [] ->
            false
    end.

get_subscriptions(Queue) ->
    Subs = [ets:lookup(?MODULE, Q) || Q <- gen_queue_hierarchy(Queue)],
    [{I, D} || {_, I, D} <- lists:append(Subs)].

clean_subscriptions(Pid) ->
    Subs = ets:lookup(?MODULE, Pid),
    ets:delete(?MODULE, Pid),
    lists:foreach(fun({_, Q, I, D}) -> ets:delete_object(?MODULE, {Q, I, D}) end, Subs).

gen_queue_hierarchy(Queue) ->
    [{0, _} | Matches] = binary:matches(Queue, <<$/>>),
    [Queue | gen_queue_hierarchy(Queue, Matches)].

gen_queue_hierarchy(Queue, Matches) ->
    [binary:part(Queue, 0, Pos) || {Pos, _} <- Matches].
