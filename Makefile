REBAR_URL=http://github.com/downloads/basho/rebar/rebar

rebar:
	wget $(REBAR_URL)
	chmod +x rebar

deps:
	./rebar get-deps

all: rebar deps
	./rebar compile
