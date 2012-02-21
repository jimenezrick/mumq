-module(mumq_pers).

%-behaviour(gen_server).

-export([enqueue/2,
         acknowledge/3,
         send_unread_messages/3]).

%%%-----------------------------------------------------------------------------
%%% XXX: Fake API, actualizar mumq.app.src!
%%%-----------------------------------------------------------------------------

enqueue(_Queue, Msg) ->
    mumq_queue:enqueue(Msg).

acknowledge(_Queue, SubId, MsgId) ->
    mumq_queue:acknowledge(SubId, MsgId).

send_unread_messages(_Queue, SubId, To) ->
    mumq_queue:send_unread_messages(SubId, To).
