#!/bin/sh

erl -pa $(dirname $0)/../ebin        \
    -pa $(dirname $0)/../deps/*/ebin \
    -run mumq_benchmark              \
