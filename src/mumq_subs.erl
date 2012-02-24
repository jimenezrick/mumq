-module(mumq_subs).

-export([create_table/0,
         add_subscription/3,
         del_subscription/3,
         del_subscriptions/1,
         get_subscriptions/1,
         split_queue_name/1]).

-define(ETS_OPTS, [bag,
                   named_table,
                   public,
                   {read_concurrency, true}]).

create_table() ->
    ets:new(?MODULE, ?ETS_OPTS).

add_subscription(Queue, Id, DeliveryProc) ->
    Queue2 = split_queue_name(Queue),
    case ets:match_object(?MODULE, {self(), Queue2, Id, DeliveryProc}) of
        [] ->
            ets:insert(?MODULE, [{self(), Queue2, Id, DeliveryProc},
                                 {Queue2, Id, DeliveryProc}]);
        [_] ->
            false
    end.

del_subscription(Queue, Id, DeliveryProc) ->
    Queue2 = split_queue_name(Queue),
    case
        ets:select_delete(?MODULE, [{{Queue2, Id, DeliveryProc}, [], [true]},
                                    {{self(), Queue2, Id, DeliveryProc}, [], [true]}])
    of
        2 -> true;
        0 -> false
    end.

get_subscriptions(Queue) ->
    Match = [{{Q, '$1', '$2'}, [], [{{'$1', '$2'}}]} ||
            Q <- make_queue_hierarchy(Queue)],
    ets:select(?MODULE, Match).

del_subscriptions(Pid) ->
    Match = [{{Q, I, D}, [], [true]} || {_, Q, I, D} <- ets:lookup(?MODULE, Pid)],
    ets:select_delete(?MODULE, Match),
    ets:delete(?MODULE, Pid).

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
