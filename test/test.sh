#!/bin/sh

PORT=61613

if [ $# != 1 ]
then
	echo "Usage: $0 <frames>" >&2
	exit 1
fi

nc localhost $PORT <$1
