
# Fault injection for shared/dynamic library function calls

## Prerequisites

* Erlang/OTP R16B02 or later.  The library will probably work for
  releases R15B and earlier, but I haven't tried it yet.

* OS X.  I fully intend to support Linux and Solaris/SmartOS/OmniOS
  and FreeBSD, but I haven't tested it yet.

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

Now, let's use it and see what happens.

    % touch foofoo
    
    % env `ebin/example_environment.sh yo` /bin/rm foofoo
    rm: foofoo: Too many levels of remote in path
    
    % env `ebin/example_environment.sh yo` /bin/rm foofoo
    rm: foofoo: Too many levels of remote in path
    
    % ls -l foofoo
    -rw-rw-r--  1 fritchie  staff  0 Feb 10 16:50 foofoo

Yay, we cannot delete the file.  The intercept library is definitely
lying to us.

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


