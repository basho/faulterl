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

-module(trigger_switchpanel).
-export([config/0]).

-include("faulterl.hrl").

config() ->
    #fi{
        name = "switchpanel",
        type = trigger,
        c_decl_extra = ["#define SWITCHPANEL_SIZE 128",
                        "u_int8_t  bc_fi_switchpanel_array[SWITCHPANEL_SIZE];",
                        "u_int8_t  bc_fi_switchpanel_default = 1;"],
        extra_global_syms = ["bc_fi_switchpanel_array",
                             "bc_fi_switchpanel_default"],
        trigger_struct = "
typedef struct {
    char *i_name; /* Must be present */
    int index;
}",
        trigger_init = "
    int i;

    for (i = 0; i < SWITCHPANEL_SIZE; i++) {
        bc_fi_switchpanel_array[i] = bc_fi_switchpanel_default;
    }
",
        trigger_new_instance_args = "int index",
        trigger_new_instance_body = "
    a->index = index;
",
        trigger_body = "
    res = bc_fi_switchpanel_array[a->index];
"
    }.

