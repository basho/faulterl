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

-module(trigger_args_of_intercept).
-export([config/14]).

-include("faulterl.hrl").

config(CHeaders, CDeclExtra, ExtraGlobalSyms,
       Enabled, TDisabled, Verbose,
       TStruct, TInit, TNewInstanceArgs, TNewInstanceBody,
       TWithInterceptsArgs, TBody,
       InterceptName, InterceptSuffix) ->
    #fi{
        name = "i_arg_" ++ InterceptName ++ "_" ++ InterceptSuffix,
        type = trigger,
        c_headers = CHeaders,
        c_decl_extra = CDeclExtra,
        extra_global_syms = ExtraGlobalSyms,
        enabled = Enabled,
        trigger_disabled = TDisabled,
        trigger_struct = TStruct,
        trigger_init = TInit,
        trigger_new_instance_args = TNewInstanceArgs,
        trigger_new_instance_body = TNewInstanceBody,
        trigger_with_intercepts_args = TWithInterceptsArgs,
        trigger_body = TBody,
        verbose = Verbose
   }.



