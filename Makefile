REBAR = $(shell which rebar || echo ./rebar)
ENABLE_STATIC = no

all: deps-up eimap

deps:
	rebar get-deps

deps-up: deps
	rebar update-deps

eimap:
	ENABLE_STATIC=no rebar compile

tests:
	rebar eunit

run:
	erl -pa ebin deps/*/ebin


