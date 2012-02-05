REBAR_URL ?= http://github.com/downloads/basho/rebar/rebar

ifneq ($(shell which wget 2>/dev/null),)
REBAR_GET ?= wget -q $(REBAR_URL)
else
REBAR_GET ?= curl -s -f $(REBAR_URL) >rebar
endif

.PHONY: all deps release clean clean-all

all: rebar deps
	./rebar compile

rebar:
	$(REBAR_GET)
	chmod +x rebar

deps: rebar
	./rebar get-deps

release: rebar all
	./rebar generate

clean: rebar
	./rebar clean

clean-all:
	rm -rf rebar ebin deps rel/mumq log doc
