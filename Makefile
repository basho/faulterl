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

compile:
	$(REBAR_BIN) compile

deps:
	$(REBAR_BIN) get-deps

clean:
	$(REBAR_BIN) clean

test: deps compile eunit

eunit:
	$(REBAR_BIN) -v skip_deps=true eunit

NOW	= $(shell date +%s)
COUNTER = $(PWD)/$(NOW).current_counterexample.eqc
EQCINFO = $(PWD)/$(NOW).eqc-info

