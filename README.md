
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

Second, compile a fault injection XML specification file.

In this example, we will experiment by injecting faults into the
`unlink(2)` and `unlinkat(2)` system calls.  We tell the `libfi`
program to intercept those system calls when running the `/bin/rm`
program.

    % touch /tmp/sample-file
    
    % (cd ./deps/lfi ; ./libfi ../../priv/scenario/unlink.xml)
    rm: /tmp/sample-file: Inappropriate ioctl for device
    Process exited normally. Exit status: 1
    
    % ls -l /tmp/sample-file
    -rw-rw-r--  1 fritchie  staff  0 Feb 10 16:50 /tmp/sample-file

Hooray, we cannot delete the file.  The intercept library is definitely
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

    1> faulterl_nif:peek8("g_libfi_enabled").
    1
    
    2> faulterl_nif:peek8("symbol_does_not_exist").
    not_found

The value of the `g_libfi_enabled` global symbol, which is an 8 bit
variable (C `u_int8_t` type), is `1`.  Let's check to see that our
intercept library is indeed still working.

    3> file:delete("foofoo").
    {error,enotty}

Yes, it's working.  Now we set `g_libfi_enabled` to zero, which will
disable all intercepts in our library.

    7> faulterl_nif:poke8("g_libfi_enabled", 0).
    ok
    
    8> faulterl_nif:peek8("g_libfi_enabled").
    0

Good, `g_libfi_enabled` has changed value from `1` -> `0`.  Let's see if
the intercept is really disabled.

    9> file:delete("foofoo").
    ok
    
    10> file:delete("foofoo").
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


