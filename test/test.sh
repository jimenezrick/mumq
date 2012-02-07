#!/bin/sh

PORT=61613

nc localhost $PORT <$(dirname $0)/frames
