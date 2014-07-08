REPO		?= faulterl
FAULTERL_TAG	 = $(shell git describe --tags)
REVISION	?= $(shell echo $(FAULTERL_TAG) | sed -e 's/^$(REPO)-//')
PKG_VERSION	?= $(shell echo $(REVISION) | tr - .)
BASE_DIR         = $(shell pwd)
REBAR_BIN := $(shell which rebar)
ifeq ($(REBAR_BIN),)
REBAR_BIN = ./rebar
endif

.PHONY: rel deps package pkgclean

all: deps compile

# rebar_post_compile is typically only used by Rebar's post_hooks.
rebar_post_compile: compile_scripts compile_scenarios

deps:
	./rebar get-deps

compile:
	$(REBAR_BIN) compile

clean:
	$(REBAR_BIN) clean
	rm -f ebin/*.escript
	rm -f ebin/*.sh
	(cd priv/lfi ; make clean)

compile_scripts: ebin/example_environment.sh

compile_scenarios:
	erlc +debug_info -o ebin -I include priv/scenario/*erl

ebin/example_environment.sh: priv/scripts/example_environment.sh
	cp -p $< $@

test: compile eunit

eunit:
	$(REBAR_BIN) -v skip_deps=true eunit
