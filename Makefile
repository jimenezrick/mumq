REBAR_URL ?= http://cloud.github.com/downloads/basho/rebar/rebar

ifneq ($(shell which wget 2>/dev/null),)
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

rel: all
	mkdir -p rel
	cd rel; ../rebar create-node nodeid=mumq_node
	./rebar generate

clean:
	./rebar clean

clean-all:
	rm -rf rebar ebin deps rel
