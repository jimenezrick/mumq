-module(mumq_queue).

-behaviour(gen_server).

-export([start_link/1,
         enqueue_message/2,
         acknowledge_message/3,
         send_unread_messages/3,
         queue_info/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("mumq.hrl").

-record(state, {name,
                max_qsize,
                qsize = 0,
                queue = queue:new(),
                next_seq = 0,
                msg_seqs = gb_trees:empty(),
                sub_seqs = gb_trees:empty()}).

start_link(QueueName) ->
    gen_server:start_link(?MODULE, QueueName, []).

enqueue_message(To, Msg) ->
    gen_server:call(To, {enqueue, Msg}).

acknowledge_message(To, SubId, MsgId) ->
    gen_server:cast(To, {acknowledge, SubId, MsgId}).

send_unread_messages(To, SubId, SendTo) ->
    gen_server:cast(To, {send_unread, SubId, SendTo}).

queue_info(To) ->
    gen_server:call(To, info).

init(QueueName) ->
    case application:get_env(max_queue_inactivity) of
        undefined ->
            MaxQueueInactivity = timer:minutes(?MAX_QUEUE_INACTIVITY);
        {ok, MaxQueueInactivity0} ->
            MaxQueueInactivity = timer:minutes(MaxQueueInactivity0)
    end,
    case application:get_env(subscribers_purge_interval) of
        undefined ->
            SubscribersPurgeInterval = timer:minutes(?SUBSCRIBERS_PURGE_INTERVAL);
        {ok, SubscribersPurgeInterval0} ->
            SubscribersPurgeInterval = timer:minutes(SubscribersPurgeInterval0)
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
    {ok, #state{name = QueueName, max_qsize = MaxQueueSize}}.

handle_call({enqueue, Msg}, _From, State) ->
    Seq = State#state.next_seq,
    Queue = queue:in({Seq, Msg}, State#state.queue),
    MsgId = mumq_stomp:get_header(Msg, <<"message-id">>),
    MsgSeqs = gb_trees:insert(MsgId, Seq, State#state.msg_seqs),
    if
        State#state.qsize == State#state.max_qsize ->
            Size = State#state.qsize,
            {{value, {_, MsgDrop}}, Queue2} = queue:out(Queue),
            MsgIdDrop = mumq_stomp:get_header(MsgDrop, <<"message-id">>),
            MsgSeqs2 = gb_trees:delete(MsgIdDrop, MsgSeqs);
        true ->
            Size = State#state.qsize + 1,
            Queue2 = Queue,
            MsgSeqs2 = MsgSeqs
    end,
    {reply, ok, State#state{qsize = Size, queue = Queue2,
                            next_seq = Seq + 1, msg_seqs = MsgSeqs2}};
handle_call(info, _From, State) ->
    Queue = queue:to_list(State#state.queue),
    Subs = gb_trees:to_list(State#state.sub_seqs),
    {reply, {Queue, Subs}, State}.

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
handle_cast({send_unread, SubId, SendTo}, State) ->
    case gb_trees:lookup(SubId, State#state.sub_seqs) of
        {value, AckSeq} ->
            true;
        none ->
            case queue:peek(State#state.queue) of
                {value, {Seq, _}} ->
                    AckSeq = Seq;
                empty ->
                    AckSeq = 0
            end
    end,
    queue_map_from(
        fun(M) ->
                SendTo ! mumq_stomp:add_header(M, <<"subscription">>, SubId)
        end, State#state.queue, AckSeq),
    {noreply, State}.

handle_info({max_queue_inactivity, _, PrevSeq}, State = #state{next_seq = PrevSeq}) ->
    lager:info("Dropping stored messages in ~s due to queue inactivity",
               [State#state.name]),
    {stop, normal, State};
handle_info({max_queue_inactivity, T, _}, State) ->
    erlang:send_after(T, self(), {max_queue_inactivity, T, State#state.next_seq}),
    {noreply, State};
handle_info({subscribers_purge_interval, T}, State) ->
    lager:info("Purging subscribers from queue ~s", [State#state.name]),
    State2 = purge_subscribers(State),
    erlang:send_after(T, self(), {subscribers_purge_interval, T}),
    {noreply, State2}.

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

purge_subscribers(State) ->
    case queue:peek(State#state.queue) of
        {value, {Seq, _}} ->
            SubSeqs = State#state.sub_seqs,
            SubSeqs2 = purge_subscribers(Seq, gb_trees:iterator(SubSeqs), SubSeqs),
            State#state{sub_seqs = SubSeqs2};
        empty ->
            State#state{sub_seqs = gb_trees:empty()}
    end.

purge_subscribers(FirstSeq, IterSubSeqs, SubSeqs) ->
    case gb_trees:next(IterSubSeqs) of
        {SubId, AckSeq, IterSubSeqs2} when AckSeq < FirstSeq ->
            SubSeqs2 = gb_trees:delete(SubId, SubSeqs),
            purge_subscribers(FirstSeq, IterSubSeqs2, SubSeqs2);
        {_, _, IterSubSeqs2} ->
            purge_subscribers(FirstSeq, IterSubSeqs2, SubSeqs);
        none ->
            SubSeqs
    end.
