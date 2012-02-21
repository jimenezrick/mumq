-module(mumq_pers).

%-behaviour(gen_server).

-export([enqueue_message/2,
         acknowledge_message/3,
         send_unread_messages/3]).

%%%-----------------------------------------------------------------------------
%%% XXX: Fake API, actualizar mumq.app.src!
%%%
%%% TODO: gen_server como mumq_subs?
%%% TODO: Mirar mumq_subs, a ver si con una sola ETS queda bien.
%%%-----------------------------------------------------------------------------

enqueue_message(_Queue, Msg) ->
    mumq_queue:enqueue_message(Msg).

acknowledge_message(_Queue, SubId, MsgId) ->
    mumq_queue:acknowledge_message(SubId, MsgId).

send_unread_messages(_Queue, SubId, To) ->
    mumq_queue:send_unread_messages(SubId, To).
