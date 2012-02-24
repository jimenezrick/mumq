-module(mumq_pers).

-behaviour(gen_server).

-export([start_link/0,
         enqueue_message/2,
         acknowledge_message/3,
         send_unread_messages/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(ETS_OPTS, [ordered_set,
                   named_table,
                   public,
                   {read_concurrency, true}]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

enqueue_message(Queue, Msg) ->
    mumq_queue:enqueue_message(lookup_queue(Queue), Msg).

acknowledge_message(Queue, SubId, MsgId) ->
    mumq_queue:acknowledge_message(lookup_queue(Queue), SubId, MsgId).

send_unread_messages(Queue, SubId, SendTo) ->
    lists:foreach(
        fun(Q) ->
                mumq_queue:send_unread_messages(Q, SubId, SendTo)
        end, lookup_nested_queues(Queue)).

init(_Args) ->
    ets:new(?MODULE, ?ETS_OPTS),
    {ok, gb_trees:empty()}.

handle_call({register_queue, Queue}, _From, Queues) ->
    {ok, Pid} = mumq_qsup:start_child(),
    Queue2 = mumq_subs:split_queue_name(Queue),
    case ets:insert_new(?MODULE, {Queue2, Pid}) of
        true ->
            monitor(process, Pid),
            Queues2 = gb_trees:insert(Pid, Queue2, Queues),
            {reply, Pid, Queues2};
        false ->
            mumq_qsup:terminate_child(Pid),
            {reply, retry, Queues}
    end.

handle_cast(_Req, _State) ->
    exit(not_implemented).

handle_info({'DOWN', _, process, Pid, _}, Queues) ->
    Queue = gb_trees:get(Pid, Queues),
    Queues2 = gb_trees:delete(Pid, Queues),
    ets:delete(?MODULE, Queue),
    {noreply, Queues2}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, _State, _Extra) ->
    exit(not_implemented).

lookup_queue(Queue) ->
    Queue2 = mumq_subs:split_queue_name(Queue),
    case ets:lookup(?MODULE, Queue2) of
        [{_, Pid}] ->
            Pid;
        [] ->
            case gen_server:call(?MODULE, {register_queue, Queue}) of
                retry ->
                    lookup_queue(Queue);
                Pid ->
                    Pid
            end
    end.

lookup_nested_queues(Queue) ->
    Queue2 = mumq_subs:split_queue_name(Queue),
    ets:select(?MODULE, [{{Queue2 ++ '_', '$1'}, [], ['$1']}]).
