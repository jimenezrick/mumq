-module(mumq_conn).

-export([handle_connection/2]).

handle_connection(_Socket, _State) ->
    io:format("$ancestors = ~p~n", [get('$ancestors')]),
    timer:sleep(timer:seconds(3)).
