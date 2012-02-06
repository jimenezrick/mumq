#!/bin/sh

erl -pa ebin                     \
    -pa deps/*/ebin              \
    -boot start_sasl             \
    -config rel/files/app.config \
    -args_file rel/files/vm.args \
    -run mumq                    \
    $*
