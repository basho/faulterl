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

all: compile

compile: ebin/make_intercept_c.escript \
         ebin/example_environment.sh
	$(REBAR_BIN) compile
	erlc -o ebin -I include scenario/*erl

clean:
	$(REBAR_BIN) clean
	rm -f ebin/*.escript
	rm -f ebin/*.sh

test: compile eunit

eunit:
	$(REBAR_BIN) -v skip_deps=true eunit

NOW	= $(shell date +%s)
COUNTER = $(PWD)/$(NOW).current_counterexample.eqc
EQCINFO = $(PWD)/$(NOW).eqc-info

ebin/make_intercept_c.escript: priv/scripts/make_intercept_c.escript
	echo "#!/usr/bin/env escript" > $@
	echo "%%! -pz $(PWD)/ebin" >> $@
	cat $< >> $@
	chmod +x $@

ebin/example_environment.sh: priv/scripts/example_environment.sh
	cp -p $< $@
