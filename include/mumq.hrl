-define(TCP_PORT,                   61613).
-define(TCP_ACCEPTORS,              4).
-define(MAX_FRAME_SIZE,             8 * 1024).
-define(MAX_QUEUE_SIZE,             100).
-define(MAX_QUEUE_INACTIVITY,       7 * 24 * 60).
-define(SUBSCRIBERS_PURGE_INTERVAL, 60).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

-record(frame, {cmd, headers = [], body = <<>>}).
