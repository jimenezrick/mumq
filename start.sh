#!/bin/sh

erl -pa deps/*/ebin -boot start_sasl -s mumq_app
