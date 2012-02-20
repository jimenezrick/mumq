-define(TCP_PORT,                   61613).
-define(TCP_ACCEPTORS,              4).
-define(MAX_FRAME_SIZE,             4 * 1024 * 1024).
-define(MAX_QUEUE_SIZE,             100).
-define(MAX_QUEUE_INACTIVITY,       7 * 24 * 60).
-define(SUBSCRIBERS_PURGE_INTERVAL, 60).

-record(frame, {cmd, headers = [], body = <<>>}).
