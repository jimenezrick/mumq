-module(mumq_subs).

-export([create_table/0,
         add_subscription/3,
         del_subscription/3,
         del_subscriptions/1,
         get_subscriptions/1,
         subscribed_clients/0]).

-define(ETS_OPTS, [bag,
                   named_table,
                   public,
                   {read_concurrency, true}]).

create_table() ->
    ets:new(?MODULE, ?ETS_OPTS).

add_subscription(Queue, Id, DeliveryProc) ->
    case ets:match_object(?MODULE, {self(), Queue, Id, DeliveryProc}) of
        [] ->
            ets:insert(?MODULE, [{self(), Queue, Id, DeliveryProc},
                                 {Queue, Id, DeliveryProc}]);
        [_] ->
            false
    end.

del_subscription(Queue, Id, DeliveryProc) ->
    case
        ets:select_delete(?MODULE, [{{Queue, Id, DeliveryProc}, [], [true]},
                                    {{self(), Queue, Id, DeliveryProc}, [], [true]}])
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

make_queue_hierarchy(Queue) ->
    [{0, _} | Matches] = binary:matches(Queue, <<"/">>),
    [Queue | lists:reverse(make_queue_hierarchy(Queue, Matches))].

make_queue_hierarchy(Queue, Matches) ->
    [binary:part(Queue, 0, Pos) || {Pos, _} <- Matches].

subscribed_clients() ->
    [{P, Q, I} || {P, Q, I, _} <- ets:tab2list(?MODULE)].
