-module(mumq_subs).

-export([init/0,
         terminate/0,
         add_subscriber/2,
         del_subscriber/2,
         get_subscriber/1]).

init() ->
    ets:new(?MODULE, [named_table, bag, public, {read_concurrency, true}]).

terminate() ->
    ets:delete(?MODULE).

add_subscriber(Channel, Pid) ->
    % TODO: Use ets:insert(Tab, [Obj]), is atomic and isolated
    ets:insert(?MODULE, {Channel, Pid}).

del_subscriber(Channel, Pid) ->
    ets:delete_object(?MODULE, {Channel, Pid}).

get_subscriber(Channel) ->
    try
        Pids = ets:lookup_element(?MODULE, Channel, 2),
        io:format("Pids = ~p~n", [Pids])
    catch
        error:badarg ->
            ok
    end.
