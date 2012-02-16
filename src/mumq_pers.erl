-module(mumq_pers).

-behaviour(gen_server).

-compile(export_all).

-define(ETS_OPTS, [ordered_set,
                   named_table,
                   public,
                   {read_concurrency, true}]).

-record(state, {queue = queue:new(),
                qsize = 0,
                max_qsize = 1000, % XXX
                next_seq = 0,
                subscriptions = gb_trees:empty()}).





%%%---------------------------------------------------------------------------------
%%% TODO: Cuando una cola se queda vacia, arrancar un timer y destruir la cola
%%%       si no se ha recibido ningun mensaje mas.
%%% TODO: Crear otro modulo mumq_queue con los gen_server, que guardan los mensajes.
%%%       Usar este modulo solo para lo mismo que mumq_subs, limpiar la ETS y
%%%       supervisar a los mumq_queue.
%%%---------------------------------------------------------------------------------


send_unread_messages(Queue, To) ->
    SubQueues = search_subqueues(Queue),
    % TODO: gen_server cast
    lists:foreach(fun(Q) -> mumq_queue:send_unread_messages(Q, To) end, SubQueues).



search_subqueues(Queue) ->
    case ets:lookup(?MODULE, Queue) of
        [Obj] ->
        [] ->
            end.

more(Queue, Key) ->
    case ets:next(?MODULE, Key) of
        Queue2 ->
            case binary:part(Queue2, 0, size(Queue) + 1) of
                <<Queue/binary, $/>> ->
                    ok;
                _ ->
                    error
            end;
        '$end_of_table' ->





init(_Args) ->
    process_flag(trap_exit, true),
    ets:new(?MODULE, ?ETS_OPTS),
    {ok, #state{}}.







handle_call({ack, SubId, MsgId}, From, State) ->
        Reply = ok,
        {reply, Reply, State}.

handle_cast({enqueue, Frame}, State = #state{qsize = N, max_qsize = N}) ->
    Seq = State#state.next_seq,
    Queue = queue:in({Seq, Frame}, queue:drop(State#state.queue)),
    {noreply, State#state{queue = Queue, next_seq = Seq + 1}};
handle_cast({enqueue, Frame}, State = #state{qsize = N}) ->
    Seq = State#state.next_seq,
    Queue = queue:in({Seq, Frame}, State#state.queue),
    {noreply, State#state{queue = Queue, qsize = N + 1, next_seq = Seq + 1}}.

ack_message(State, SubId, MsgId) ->
    case gb_trees:lookup(SubId, State#state.subscriptions) of
        {value, Seq} ->
            {ok, gb_trees:update(SubId, Seq + 1, State#state.subscriptions)};
        none ->
            {error, not_subscribed}
    end.

take_stored_messages(State, SubId) ->
    case gb_trees:lookup(SubId, State#state.subscriptions) of
        {value, Seq} ->
            NumMsgs = State#state.next_seq - Seq,
            DropMsgs = State#state.qsize - NumMsgs,
            {_, Msgs} = queue:split(DropMsgs, State#state.queue),
            {ok, Msgs};
        none ->
            {error, not_subscribed}
    end.
