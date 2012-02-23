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

%%%----------------------------------
%%% TODO: Meterlo en el supervisor
%%%----------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, self(), []).

init(SupPid) ->
    process_flag(trap_exit, true),
    ets:new(?MODULE, ?ETS_OPTS),
    {ok, SupPid}.

handle_call(_Req, _From, _State) ->
    exit(not_implemented).

handle_cast(_Req, _State) ->
    exit(not_implemented).

%% XXX XXX XXX
handle_info({'EXIT', SupPid, Reason}, SupPid) ->
    exit(Reason);
handle_info({'EXIT', Pid, _Reason}, SupPid) ->
    clean_subscriptions(Pid),
    {noreply, SupPid}.
%% XXX XXX XXX

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, _State, _Extra) ->
    exit(not_implemented).





enqueue_message(_Queue, Msg) ->
    mumq_queue:enqueue_message(Msg).

acknowledge_message(_Queue, SubId, MsgId) ->
    mumq_queue:acknowledge_message(SubId, MsgId).

send_unread_messages(_Queue, SubId, To) ->
    mumq_queue:send_unread_messages(SubId, To).



lookup_queue_process(Queue) ->
    case ets:lookup(?MODULE, Queue) of
        [{Queue, Pid}] ->
            Pid;
        [] ->
            % TODO: Hacerlo atraves del gen_server para serializar
            Pid = start_queue_process(Queue),
            % TODO: Hacerlo en el gen_server
            %ets:insert(?MODULE, {Queue, Pid})
    end.
