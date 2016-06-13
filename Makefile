REBAR = $(shell which rebar3 || echo ./rebar3)
ENABLE_STATIC = no

all:
	ENABLE_STATIC=$(ENABLE_STATIC) $(REBAR) compile

tests:
	$(REBAR) eunit

run:
	$(REBAR) shell --apps eimap


