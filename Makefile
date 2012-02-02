REBAR_URL ?= http://cloud.github.com/downloads/basho/rebar/rebar

ifeq ($(words $(shell which wget)), 1)
REBAR_GET ?= wget -q $(REBAR_URL)
else
REBAR_GET ?= curl -s -f $(REBAR_URL) >rebar
endif

.PHONY: all clean clean-all

all: rebar deps
	./rebar compile

rebar:
	$(REBAR_GET)
	chmod +x rebar

deps:
	./rebar get-deps

clean:
	./rebar clean

clean-all:
	rm -rf ebin deps rebar
