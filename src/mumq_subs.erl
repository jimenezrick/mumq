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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    ets:new(?MODULE, ?ETS_OPTS),
    {ok, none}.

handle_call(_Req, _From, _State) ->
    exit(not_implemented).

handle_cast(_Req, _State) ->
    exit(not_implemented).

handle_info({'EXIT', Pid, _Reason}, State) ->
    clean_subscriptions(Pid),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, _State, _Extra) ->
    exit(not_implemented).

link() ->
    link(whereis(?MODULE)),
    put('$ancestors', [?MODULE | get('$ancestors')]).

add_subscription(Queue, Id, DeliveryProc) ->
    Queue2 = split_queue_name(Queue),
    case ets:match_object(?MODULE, {self(), Queue2, Id, DeliveryProc}) of
        [] ->
            ets:insert(?MODULE, {Queue2, Id, DeliveryProc}),
            ets:insert(?MODULE, {self(), Queue2, Id, DeliveryProc});
        [_] ->
            false
    end.

del_subscription(Queue, Id, DeliveryProc) ->
    Queue2 = split_queue_name(Queue),
    case ets:match_object(?MODULE, {self(), Queue2, Id, DeliveryProc}) of
        [_] ->
            ets:delete_object(?MODULE, {Queue2, Id, DeliveryProc}),
            ets:delete_object(?MODULE, {self(), Queue2, Id, DeliveryProc});
        [] ->
            false
    end.

get_subscriptions(Queue) ->
    Match = [{{Q, '$1', '$2'}, [], [{{'$1', '$2'}}]} || Q <- make_queue_hierarchy(Queue)],
    ets:select(?MODULE, Match).

clean_subscriptions(Pid) ->
    Match = [{{Q, I, D}, [], [true]} || {_, Q, I, D} <- ets:lookup(?MODULE, Pid)],
    ets:delete(?MODULE, Pid),
    ets:select_delete(?MODULE, Match).

split_queue_name(Queue) ->
    [<<>> | Parts] = binary:split(Queue, <<"/">>, [global]),
    Parts.

make_queue_hierarchy(Queue) ->
    Parts = split_queue_name(Queue),
    [lists:reverse(drop(N, lists:reverse(Parts))) ||
        N <- lists:seq(0, length(Parts) - 1)].

drop(0, L) ->
    L;
drop(N, [_ | T]) ->
    drop(N - 1, T).
