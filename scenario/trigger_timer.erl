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

-module(trigger_timer).
-export([config/0]).

-include("faulterl.hrl").

config() ->
    #fi{
        name = "timer",
        type = trigger,
        c_headers = ["<time.h>"],
        c_decl_extra = ["u_int32_t bc_fi_timer_start_time = 0;"],
        extra_global_syms = ["bc_fi_timer_start_time"],
        trigger_struct = "
typedef struct {
    char *i_name; /* Must be present */
    time_t delay;
}",
        trigger_init = "
    bc_fi_timer_start_time = time(NULL);
",
        trigger_new_instance_args = "time_t delay",
        trigger_new_instance_body = "
    a->delay = delay;
",
        trigger_body = "
    if (time(NULL) - (time_t)bc_fi_timer_start_time > a->delay) {
        res = 1;
    } else {
        res = 0;
    }
"
    }.
