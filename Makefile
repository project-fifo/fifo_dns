REBAR = $(shell pwd)/rebar3
ELVIS = $(shell pwd)/elvis
APP=fifo_dns

.PHONY: rel stagedevrel package all tree

all: version_header compile

include fifo.mk

version_header:

clean:
	$(REBAR) clean
	$(MAKE) -C rel/pkg clean

long-test:
	$(REBAR) as eqc,long eunit
