-module(mumq_queue).

-behaviour(gen_server).

-export([start_link/0,
         enqueue_message/2,
         send_unread_messages/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("mumq.hrl").

-record(state, {max_qsize,
                qsize = 0,
                queue = queue:new(),
                next_seq = 0,
                msg_seqs = gb_trees:empty(),
                sub_acks = gb_trees:empty()}).

%%%--------------------------------------------------------------------------
%%% TODO: Implementar el borrado pasado 1 dia de las subscripciones no usadas
%%%--------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

enqueue_message(_Queue, Msg) ->
    gen_server:cast(?MODULE, {enqueue, Msg}).

send_unread_messages(_, _, undefined) ->
    ok;
send_unread_messages(To, _Queue, SubId) ->
    gen_server:cast(?MODULE, {send_unread, To, SubId}).

init(_Args) ->
    case application:get_env(max_queue_size) of
        undefined ->
            {ok, #state{max_qsize = ?MAX_QUEUE_SIZE}};
        {ok, Max} ->
            {ok, #state{max_qsize = Max}}
    end.

handle_call(_Req, _From, _State) ->
    exit(not_implemented).

handle_cast({enqueue, Msg}, State) ->
    Size = State#state.qsize,
    Seq = State#state.next_seq,
    Queue = queue:in({Seq, Msg}, State#state.queue),
    MsgId = mumq_stomp:get_header(Msg, <<"message-id">>),
    MsgSeqs = gb_trees:insert(MsgId, Seq, State#state.msg_seqs),
    if
        State#state.qsize == State#state.max_qsize ->
            {{value, MsgDrop}, Queue2} = queue:out(Queue),
            MsgIdDrop = mumq_stomp:get_header(MsgDrop, <<"message-id">>),
            MsgSeqs2 = gb_trees:delete(MsgIdDrop, MsgSeqs);
        true ->
            Queue2 = Queue,
            MsgSeqs2 = MsgSeqs
    end,
    {noreply, State#state{qsize = Size + 1, queue = Queue2,
                          next_seq = Seq + 1, msg_seqs = MsgSeqs2}};
handle_cast({send_unread, To, SubId}, State) ->
    queue_foreach(
        fun({_, M}) ->
                To ! mumq_stomp:add_header(M, <<"subscription">>, SubId)
        end, State#state.queue),
    {noreply, State}.

handle_info(_Info, _State) ->
    exit(not_implemented).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, _State, _Extra) ->
    exit(not_implemented).

queue_foreach(Fun, Queue) ->
    case queue:out(Queue) of
        {{value, Item}, Queue2} ->
            Fun(Item),
            queue_foreach(Fun, Queue2);
        {empty, _} ->
            ok
    end.
