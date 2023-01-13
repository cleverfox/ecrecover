REBAR3 ?= $(shell test -e `which rebar3` 2>/dev/null && which rebar3 || echo "./rebar3")

all: compile

compile:
	${REBAR3} compile

clean:
	${REBAR3} clean

test: compile
	${REBAR3} eunit