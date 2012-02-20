-module(mumq_queue).

-behaviour(gen_server).

-export([start_link/0,
         subscribe/1,
         enqueue/1,
         acknowledge/2,
         send_unread_messages/2]).

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
                sub_seqs = gb_trees:empty()}).

%%%-----------------------------------------------------------------------------
%%% TODO: purge_subscribers()
%%%-----------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

subscribe(SubId) ->
    gen_server:cast(?MODULE, {subscribe, SubId}).

enqueue(Msg) ->
    gen_server:cast(?MODULE, {enqueue, Msg}).

acknowledge(SubId, MsgId) ->
    gen_server:cast(?MODULE, {acknowledge, SubId, MsgId}).

send_unread_messages(To, SubId) ->
    gen_server:cast(?MODULE, {send_unread, To, SubId}).

init(_Args) ->
    case application:get_env(max_queue_inactivity) of
        undefined ->
            MaxQueueInactivity = ?MAX_QUEUE_INACTIVITY;
        {ok, MaxQueueInactivity} ->
            true
    end,
    case application:get_env(subscribers_purge_interval) of
        undefined ->
            SubscribersPurgeInterval = ?SUBSCRIBERS_PURGE_INTERVAL;
        {ok, SubscribersPurgeInterval} ->
            true
    end,
    case application:get_env(max_queue_size) of
        undefined ->
            MaxQueueSize = ?MAX_QUEUE_SIZE;
        {ok, MaxQueueSize} ->
            true
    end,
    erlang:send_after(MaxQueueInactivity, self(),
                      {max_queue_inactivity, MaxQueueInactivity, 0}),
    erlang:send_after(SubscribersPurgeInterval, self(),
                      {subscribers_purge_interval, SubscribersPurgeInterval}),
    {ok, #state{max_qsize = MaxQueueSize}}.

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
handle_cast({acknowledge, SubId, MsgId}, State) ->
    case gb_trees:lookup(MsgId, State#state.msg_seqs) of
        {value, Seq} ->
            AckSeq = Seq + 1;
        none ->
            case queue:peek(State#state.queue) of
                {value, {Seq, _}} ->
                    AckSeq = Seq;
                empty ->
                    AckSeq = 0
            end
    end,
    SubSeqs = gb_trees:enter(SubId, AckSeq, State#state.sub_seqs),
    {noreply, State#state{sub_seqs = SubSeqs}};
handle_cast({subscribe, SubId}, State) ->
    case queue:peek(State#state.queue) of
        {value, {Seq, _}} ->
            AckSeq = Seq;
        empty ->
            AckSeq = 0
    end,
    SubSeqs = gb_trees:enter(SubId, AckSeq, State#state.sub_seqs),
    {noreply, State#state{sub_seqs = SubSeqs}};
handle_cast({send_unread, To, SubId}, State) ->
    case gb_trees:lookup(SubId, State#state.sub_seqs) of
        {value, AckSeq} ->
            true;
        none ->
            case queue_foreach:peek(State#state.queue) of
                {value, {Seq, _}} ->
                    AckSeq = Seq;
                empty ->
                    AckSeq = 0
            end
    end,
    queue_map_from(
        fun(M) ->
                To ! mumq_stomp:add_header(M, <<"subscription">>, SubId)
        end, State#state.queue, AckSeq),
    {noreply, State}.

handle_info({max_queue_inactivity, _, PrevSeq}, State = #state{next_seq = PrevSeq}) ->
    {stop, normal, State};
handle_info({max_queue_inactivity, T, _}, State) ->
    erlang:send_after(T, self(), {max_queue_inactivity, T, State#state.next_seq}),
    {noreply, State};
handle_info({subscribers_purge_interval, T}, State) ->
    %
    % TODO: purge_subscribers()
    %
    erlang:send_after(T, self(), {subscribers_purge_interval, T}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, _State, _Extra) ->
    exit(not_implemented).

queue_map_from(Fun, Queue, Seq) ->
    case queue:out(Queue) of
        {{value, {Seq2, _}}, Queue2} when Seq2 < Seq ->
            queue_map_from(Fun, Queue2, Seq);
        {{value, {_, Msg}}, Queue2} ->
            Fun(Msg),
            queue_map_from(Fun, Queue2, Seq);
        {empty, _} ->
            ok
    end.
