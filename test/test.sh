#!/bin/sh

if [ $# = 0 ]
then
	echo "Usage: $0 <frames> [<arg1> [<arg2>]]" >&2
	exit 1
fi

PORT=61613
FILE=$1

shift
cat $FILE | sed "s/\$1/$1/" | sed "s/\$2/$2/" | nc localhost $PORT
