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

-record(state, {queue = queue:new(),
                qsize = 0,
                max_qsize = 1000, % XXX: Customizable
                next_seq = 0,
                subscriptions = gb_trees:empty()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

enqueue_message(_Queue, Msg) ->
    gen_server:cast(?MODULE, {enqueue, Msg}).

send_unread_messages(_, _, undefined) ->
    ok;
send_unread_messages(To, _Queue, SubId) ->
    gen_server:cast(?MODULE, {send_unread, To, SubId}).

init(_Args) ->
    {ok, #state{}}.

handle_call(_Req, _From, _State) ->
    exit(not_implemented).

handle_cast({enqueue, Msg}, State = #state{qsize = N, max_qsize = N}) ->
    Seq = State#state.next_seq,
    Queue = queue:in({Seq, Msg}, queue:drop(State#state.queue)),
    {noreply, State#state{queue = Queue, next_seq = Seq + 1}};
handle_cast({enqueue, Msg}, State = #state{qsize = N}) ->
    Seq = State#state.next_seq,
    Queue = queue:in({Seq, Msg}, State#state.queue),
    {noreply, State#state{queue = Queue, qsize = N + 1, next_seq = Seq + 1}};
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
