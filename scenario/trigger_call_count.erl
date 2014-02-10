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

-module(trigger_call_count).
-export([config/0]).

-include("faulterl.hrl").

config() ->
    #fi{
        name = "call_count",
        type = trigger,
        trigger_struct = "
typedef struct {
    char *i_name; /* Must be present */
    u_int64_t count;
    u_int64_t min;
    u_int64_t max;
}",
        trigger_init = "
    bc_fi_timer_start_time = time(NULL);
",
        trigger_new_instance_args = "u_int64_t min, u_int64_t max",
        trigger_new_instance_body = "
    a->min = min;
    a->max = max;
",
        trigger_body = "
    a->count++;
    if (a->min <= a->count && a->count <= a->max) {
        res = 1;
    } else {
        res = 0;
    }
"
    }.
