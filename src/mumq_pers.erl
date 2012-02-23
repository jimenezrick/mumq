-module(mumq_pers).

-behaviour(gen_server).

-export([start_link/0,
         link/0,
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

%%%---------------------------------------------------------------------
%%% TODO: Falta que un proceso pueda pedir los mesajes de colas anidadas
%%%---------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

link() ->
    link(whereis(?MODULE)),
    put('$ancestors', [?MODULE | get('$ancestors')]).

enqueue_message(Queue, Msg) ->
    mumq_queue:enqueue_message(lookup_queue(Queue), Msg).

acknowledge_message(Queue, SubId, MsgId) ->
    mumq_queue:acknowledge_message(lookup_queue(Queue), SubId, MsgId).

send_unread_messages(Queue, SubId, SendTo) ->
    mumq_queue:send_unread_messages(lookup_queue(Queue), SubId, SendTo).

init(_Args) ->
    process_flag(trap_exit, true),
    ets:new(?MODULE, ?ETS_OPTS),
    {ok, none}.

handle_call(_Req, _From, _State) ->
    exit(not_implemented).

handle_cast(_Req, _State) ->
    exit(not_implemented).

handle_info({'EXIT', Pid, _Reason}, State) ->
    unregister_queue(Pid),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, _State, _Extra) ->
    exit(not_implemented).

register_queue(Queue) ->
    {ok, Pid} = mumq_queue:start(),
    Queue2 = mumq_subs:split_queue_name(Queue),
    case ets:insert_new(?MODULE, {Queue2, Pid}) of
        true ->
            case ets:insert_new(?MODULE, {Pid, Queue2}) of
                true ->
                    Pid;
                false ->
                    ets:delete_object(?MODULE, {Queue2, Pid}),
                    mumq_queue:stop(Pid),
                    already_started
            end;
        false ->
            mumq_queue:stop(Pid),
            already_started
    end.

unregister_queue(Pid) ->
    case ets:lookup(?MODULE, Pid) of
        [{_, Queue}] ->
            ets:delete_object(?MODULE, {Queue, Pid}),
            ets:delete_object(?MODULE, {Pid, Queue});
        [] ->
            ok
    end.

lookup_queue(Queue) ->
    Queue2 = mumq_subs:split_queue_name(Queue),
    case ets:lookup(?MODULE, Queue2) of
        [{_, Pid}] ->
            Pid;
        [] ->
            case register_queue(Queue) of
                already_started ->
                    lookup_queue(Queue);
                Pid ->
                    Pid
            end
    end.
