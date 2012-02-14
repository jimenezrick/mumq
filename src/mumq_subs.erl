-module(mumq_subs).

-export([init/0,
         terminate/0,
         add_subscriber/2,
         del_subscriber/2,
         get_subscribers/1]).

-define(ETS_OPTS, [duplicate_bag,
                   named_table,
                   public,
                   {read_concurrency, true}]).

init() ->
    ets:new(?MODULE, ?ETS_OPTS).

terminate() ->
    ets:delete(?MODULE).

add_subscriber(Queue, Pid) ->
    ets:insert(?MODULE, {Queue, Pid}).

del_subscriber(Queue, Pid) ->
    ets:delete_object(?MODULE, {Queue, Pid}).

% XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX
get_subscribers(Queue) ->
    try
        Pids = ets:lookup_element(?MODULE, Queue, 2),
        % XXX
        io:format("Pids = ~p~n", [Pids])
        % XXX
    catch
        error:badarg ->
            error % FIXME
    end.
% XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX
