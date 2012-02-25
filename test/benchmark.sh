#!/bin/sh

erl -pa $(dirname $0)/../ebin -run mumq_benchmark
