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

# rebar_post_compile is typically only used by Rebar's post_hooks.
rebar_post_compile: compile_scripts compile_scenarios

compile:
	$(REBAR_BIN) compile

clean:
	$(REBAR_BIN) clean
	rm -f ebin/*.escript
	rm -f ebin/*.sh

compile_scripts: ebin/make_intercept_c.escript \
                 ebin/example_environment.sh

compile_scenarios:
	erlc -o ebin -I include scenario/*erl

ebin/make_intercept_c.escript: priv/scripts/make_intercept_c.escript
	echo "#!/usr/bin/env escript" > $@
	echo "%%! -pz $(PWD)/ebin" >> $@
	cat $< >> $@
	chmod +x $@

ebin/example_environment.sh: priv/scripts/example_environment.sh
	cp -p $< $@

test: compile eunit

eunit:
	$(REBAR_BIN) -v skip_deps=true eunit
