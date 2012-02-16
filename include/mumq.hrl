-define(TCP_PORT,       61613).
-define(TCP_ACCEPTORS,  5).
-define(MAX_FRAME_SIZE, 4 * 1024 * 1024).

-record(frame, {cmd, headers = [], body = <<>>}).
