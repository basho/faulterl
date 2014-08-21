
# Fault injection for shared/dynamic library function calls

## Prerequisites

* Erlang/OTP R15B01 or later.  The library may work for
  releases R14B, but I haven't tested it.  I have tested R15B01 and
  R16B02.

* OS X or Linux.  I fully intend to support Solaris/SmartOS/OmniOS
  and FreeBSD, but I haven't tested it yet.  I have tested OS X 10.8.5
  and Linux 3.2 on a Debian 7.x/"wheezy/sid" distribution.
    * A known FreeBSD limitation is that several of the lfi C++
      trigger files use `#include <execinfo.h>`, which does not exist
      on my FreeBSD 9.x test box.
    * If FreeBSD support is something that you need immediately, then
      try using the `erlang-hack-code-generator` branch of this repo.
      That branch uses the Erlang-based trigger code generator that
      I originally wrote.  It's a kludge, and it requires trigger
      and intercept files that are 100% incompatible with LFI's XML
      configuration & source files, but it does work with FreeBSD
      today.

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
    
    % (cd ./priv/lfi ; ./libfi -t '/bin/rm /tmp/sample-file' scenarios/unlink.xml)
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
`unlink.xml` scenario and run the Erlang VM using it.

    % make
    
    % (cd ./priv/lfi ; ./libfi scenarios/unlink.xml)
    
    % touch /tmp/sample-file
    
    % env `ebin/example_environment.sh $PWD/priv/lfi` erl -pz ebin

    1> faulterl_nif:peek8("symbol_does_not_exist").
    not_found
    
    2> faulterl_nif:peek8("g_libfi_enabled").
    1

The value of the `g_libfi_enabled` global symbol, which is an 8 bit
variable (C `u_int8_t` type), is `1`.  Let's check to see that our
intercept library is indeed still working.

    3> file:delete("/tmp/sample-file").
    {error,enotty}

Yes, it's working.  Now we set `g_libfi_enabled` to zero, which will
disable all intercepts in our library.

    7> faulterl_nif:poke8("g_libfi_enabled", 0).
    ok
    
    8> faulterl_nif:peek8("g_libfi_enabled").
    0

Good, `g_libfi_enabled` has changed value from `1` -> `0`.  Let's see if
the intercept is really disabled.

    9> file:delete("/tmp/sample-file").
    ok
    
    10> file:delete("/tmp/sample-file").
    {error,enoent}

Hooray!

## NIF documentation

    peek8(GlobalName::string()) -> integer() | 'not_found' | 'error'.
    peek16(GlobalName::string()) -> integer() | 'not_found' | 'error'.
    peek32(GlobalName::string()) -> integer() | 'not_found' | 'error'.
    peek64(GlobalName::string()) -> integer() | 'not_found' | 'error'.

Attempt to fetch the value of a global C/C++ variable with the name
`GlobalName` as a 8/16/32/64 bit signed integer.

    poke8(GlobalName::string(), Value:integer()) -> 'ok' | 'not_found' | 'error'.
    poke16(GlobalName::string(), Value:integer()) -> 'ok' | 'not_found' | 'error'.
    poke32(GlobalName::string(), Value:integer()) -> 'ok' | 'not_found' | 'error'.
    poke64(GlobalName::string(), Value:integer()) -> 'ok' | 'not_found' | 'error'.

Attempt to set the value of a global C/C++ variable with the name
`GlobalName` with a 8/16/32/64 bit signed integer `Value`.

Please see the EUnit test suite at the end of `src/faulterl_nifs.erl`
for example use of the full NIF library.

## libfi documentation

Please see https://github.com/dslab-epfl/lfi/wiki for documentation on
the libfi framework.

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

This library was inspired by libfi, https://github.com/dslab-epfl/lfi.
Many thanks to Paul Marinescu for fixing OS X bugs that were
introduced by changes in Apple's compiler toolchain, and many thanks
to his entire group at the Dependable Systems Lab at Ecole
polytechnique fédérale de Lausanne for creating libfi.

