#!/bin/sh

erl -pa $(dirname $0)/ebin                     \
    -pa $(dirname $0)/deps/*/ebin              \
    -boot start_sasl                           \
    -config $(dirname $0)/rel/files/app.config \
    -args_file $(dirname $0)/rel/files/vm.args \
    -run mumq                                  \
    $*
