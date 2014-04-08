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

-module(trigger_random).
-export([config/0]).

-include("faulterl.hrl").

config() ->
    #fi{
        name = "random",
        type = trigger,
        c_headers = ["<time.h>", "<string.h>"],
        c_decl_extra = ["u_int32_t bc_fi_random_seed = 0;",
                        "u_int8_t  bc_fi_random_reseed = 0;"],
        extra_global_syms = ["bc_fi_random_seed",
                             "bc_fi_random_reseed"],
        trigger_struct = "
typedef struct {
    char *i_name; /* Must be present */
    int percent;
}",
        trigger_init = "
    time_t t;

    if (!bc_fi_random_seed) {
        bc_fi_random_seed = time(&t);
    }
    srand(bc_fi_random_seed);
",
        trigger_new_instance_args = "int percent",
        trigger_new_instance_body = "
    a->percent = percent;
",
        trigger_body = "
    if (bc_fi_random_reseed) {
        srand(bc_fi_random_seed);
        bc_fi_random_reseed = 0;
    }
    res = (rand() % 100) < a->percent;
"
    }.

    
