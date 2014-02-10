%%-------------------------------------------------------------------
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

-record(fi, {
          name :: string(),
          type :: 'trigger' | 'intercept',
          c_headers = [] :: [string()],
          c_decl_extra = [] :: [string()],
          extra_global_syms = [] :: [string()],
          enabled = true :: boolean(),
          trigger_disabled = true :: boolean(),
          trigger_struct :: string(),
          trigger_init = "" :: string(),
          trigger_new_instance_args :: string(),
          trigger_new_instance_body = "" :: string(),
          trigger_body = "" :: string(),
          verbose = false :: boolean(),
          intercept_args = "" :: string(),
          intercept_errno :: 'undefined' | integer(),
          intercept_return_type :: 'undefined' | string(),
          intercept_return_value :: 'undefined' | string(),
          intercept_return_generic :: 'undefined' | string(),
          intercept_triggers = [] :: [string()]
         }).
