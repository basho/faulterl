
# Fault injection for shared/dynamic library function calls

## Prerequisites

* Erlang/OTP R15B01 or later.  The library may work for
  releases R14B, but I haven't tested it.  I have tested R15B01 and
  R16B02.

* OS X or Linux.  I fully intend to support Solaris/SmartOS/OmniOS
  and FreeBSD, but I haven't tested it yet.  I have tested OS X 10.8.5
  and Linux 3.2 on a Debian 7.x/"wheezy/sid" distribution.

* C compiler toolchain based on GCC or command-line-compatible compiler.

## Example use:

First, make everything.

    % make

Then, make the intercept shared library.  We'll give it a stupid name
with the 2nd argument: `yo`.  The first argument is the basename
(i.e. without the `.erl` extension) of the scenario that we want to
use.  The fault injection scenario files are found in the `scenario`
directory.

    % ./ebin/make_intercept_c.escript intercept_unlink yo
    egrep '^/\*--export-- ' yo.c | awk '{printf("_%s\n", $2)}' > yo.export
    gcc -g -o yo.dylib -O0 -shared -Xlinker -exported_symbols_list -Xlinker yo.export yo.c
    -rw-rw-r--  1 fritchie  staff   4728 Feb 10 16:57 yo.c
    -rwxrwxr-x  1 fritchie  staff  11020 Feb 10 16:57 yo.dylib
    drwxrwxr-x  3 fritchie  staff    102 Feb 10 16:57 yo.dylib.dSYM
    -rw-rw-r--  1 fritchie  staff    204 Feb 10 16:57 yo.export

Now, let's use it and see what happens.  Please note that many OSes
will refuse to use a pre-loaded shared library unless it has an
absolute path.  Please note that all examples below of the
`ebin/example_environment.sh` script use a `$PWD/` prefix to use an
absolute path.

    % touch foofoo
    
    % env `ebin/example_environment.sh $PWD/yo` /bin/rm foofoo
    rm: foofoo: Too many levels of remote in path
    
    % env `ebin/example_environment.sh $PWD/yo` /bin/rm foofoo
    rm: foofoo: Too many levels of remote in path
    
    % ls -l foofoo
    -rw-rw-r--  1 fritchie  staff  0 Feb 10 16:50 foofoo

Yay, we cannot delete the file.  The intercept library is definitely
lying to us.

## The NIFs: peek and poke

The scenario scheme makes it possible to create several different test
scenarios, then run each test case using an intercept library based on
each respective scenario.  However, that method can be too static for
some test cases: it would be nice if Erlang could have more control
over the behavior of the triggers.

The `peek` and `poke` NIFs in this library allow Erlang to read and
modify global-scope symbols.  Here is an example.  We build the
`trigger_unlink` scenario and start Erlang using the `yo` intercept
library.

    % make
    % ./ebin/make_intercept_c.escript intercept_unlink yo
    % env `ebin/example_environment.sh $PWD/yo` erl -pz ebin

    2> faulterl_nif:peek("bc_fi_enabled", 0, 8, false, false).
    {ok,<<1>>}

The value of the `bc_fi_enabled` global symbol, which is an 8 bit
variable (C `u_int8_t` type), is `1`.  Let's check to see that our
intercept library is indeed still working.

    3> file:delete("foofoo").
    {error,eremote}

Yes, it's working.  Now we set `bc_fi_enabled` to zero, which will
disable all intercepts in our library.

    4> faulterl_nif:peek("bc_fi_enabled", 0, 8, false, false).
    {ok,<<1>>}

    5> "Alright, we'll turn it off".
    "Alright, we'll turn it off"

    7> faulterl_nif:poke("bc_fi_enabled", 0, <<0:8/native>>, false).
    ok

    8> faulterl_nif:peek("bc_fi_enabled", 0, 8, false, false).
    {ok,<<0>>}

Good, `bc_fi_enabled` has changed value from `1` -> `0`.  Let's see if
the intercept is really disabled.

    9> file:delete("foofoo").
    real = 0x0x7f47021a7960
    ok

    10> file:delete("foofoo").
    real = 0x0x7f47021a7960
    {error,enoent}

Hooray!

The NIFs are not yet well-documented.  Please see the EUnit test suite
at the end of `src/faulterl_nifs.erl` for definitive documentation"".

## License

    %% Copyright (c) 2014 Basho Technologies, Inc. All Rights Reserved.
    %%
    %% This file is provided to you under the Apache License,
    %% Version 2.0 (the "License"); you may not use this file
    %% except in compliance with the License.  You may obtain
    %% a copy of the License at
    %%
    %%   http://www.apache.org/licenses/LICENSE-2.0
    %%
    %% Unless required by applicable law or agreed to in writing,
    %% software distributed under the License is distributed on an
    %% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
    %% KIND, either express or implied.  See the License for the
    %% specific language governing permissions and limitations
    %% under the License.

## Inspiration

This library was inspired by libfi,
https://github.com/dslab-epfl/lfi.  That library has not been actively
maintained; stability is good, but it no longer works correctly with
OS X 10.8.5.  This library uses Erlang-style syntax and a
template-style configuration instead of the original's XML syntax.

The template style is cumbersome, but it avoids the need to play games
with the stack frame and thus is much more portable than EPFL's
version.


