#!/bin/sh

erl -pa $(dirname $0)/../ebin        \
    -pa $(dirname $0)/../deps/*/ebin \
    -noshell                         \
    -run mumq_benchmark              \
