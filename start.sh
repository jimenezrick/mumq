#!/bin/sh

erl -pa ebin -pa deps/*/ebin -boot start_sasl -run mumq
