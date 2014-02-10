%% -------------------------------------------------------------------
%%
%% fault injection primitives
%%
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
%%
%% -------------------------------------------------------------------

-module(intercept_unlink).
-export([config/0]).

-include("faulterl.hrl").

config() ->
    [trigger_random:config()] ++
    [
     #fi{	% OS X version
         name = "unlink",
         type = intercept,
         intercept_args = "const char *path",
         intercept_args_call = "path",
         c_headers = ["<unistd.h>"],
         intercept_errno = "EREMOTE",
         intercept_return_type = "int",
         intercept_return_value = "-1",
         intercept_triggers = [{"random", "random_always", "100"}]
     },
     #fi{
         name = "unlinkat",	% Linux 3.2 version
         type = intercept,
         intercept_args = "int __fd, __const char *__name, int __flag",
         intercept_args_call = "__fd, __name, __flag",
         c_headers = ["<unistd.h>"],
         intercept_errno = "EREMOTE",
         intercept_return_type = "int",
         intercept_return_value = "-1",
         intercept_triggers = [{"random", "random_always", "100"}]
     }
    ].
