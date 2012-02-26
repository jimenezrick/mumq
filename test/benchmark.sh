#!/bin/sh

if [ $# != 1 ]
then
	echo "Usage: $0 <host>" >&2
	exit 1
fi

erl -pa $(dirname $0)/../ebin        \
    -pa $(dirname $0)/../deps/*/ebin \
    -noshell                         \
    -run mumq_benchmark start $1     \
    -run init stop
