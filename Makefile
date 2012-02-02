REBAR_URL=http://github.com/downloads/basho/rebar/rebar

.PHONY: all clean

all: rebar deps
	./rebar compile

rebar:
	wget $(REBAR_URL)
	chmod +x rebar

deps:
	./rebar get-deps

clean:
	rm -rf ebin
