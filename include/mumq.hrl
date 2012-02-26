-define(TCP_PORT,                   61613).
-define(TCP_ACCEPTORS,              4).
-define(MAX_FRAME_SIZE,             8 * 1024).
-define(MAX_QUEUE_SIZE,             100).
-define(MAX_QUEUE_INACTIVITY,       7 * 24 * 60).
-define(SUBSCRIBERS_PURGE_INTERVAL, 60).

-define(TCP_OPTS, [binary,
                   {active, false},
                   {packet, raw},
                   {nodelay, true},
                   {keepalive, true}]).

%-define(DEBUG, true).

-record(frame, {cmd, headers = [], body = <<>>}).
