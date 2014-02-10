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

-module(intercept_unlink_4).
-export([config/0]).

-include("faulterl.hrl").

config() ->
    [trigger_random:config()] ++
    [trigger_switchpanel:config()] ++
    [trigger_timer:config()] ++
    [trigger_call_count:config()] ++
    [
     %% Intercept unlink(), when the following are true:
     %%     * switchpanel: #42 is enabled
     %%     * random: 50% uniform random chance
     %%     * timer: a minimum of 5 seconds after start time
     %%     * call_count: true if between 50th & 60th time that all of
     %%                   the other criteria have been met.
     #fi{
         name = "unlink",
         type = intercept,
         intercept_args = "const char *path",
         intercept_args_call = "path",
         c_headers = ["<unistd.h>"],
         intercept_errno = "EREMOTE",
         intercept_return_type = "int",
         intercept_return_value = "-1",
         intercept_triggers = [{"switchpanel", "switchpanel_42", "42"},
                               {"random", "random_50", "50"},
                               {"timer", "timer_5", "5"},
                               {"call_count", "call_count_50_60", "50, 60"}]
     }
    ].
