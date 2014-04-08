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

-module(intercept_unlink_strstr).
-export([config/0]).

-include("faulterl.hrl").

%% This is a mostly-realistic example of creating a trigger
%% that examines the arguments of the intercept that would
%% be calling the trigger.
%%
%% For example, we want to have a trigger that is true if
%% the "char *path" argument matches some string.
%%
%% In this framework, a trigger is usually completely independent of
%% any intercepted function.  If we want to make a trigger that is
%% dependent on a trigger function's arguments, then we need to know
%% the number of args, the argument names, and the argument types.
%% The way to give a trigger the intercepted function's arguments is a
%% kludge, sorry, but it works.  Here's how.
%%
%% * A new intercept-arg-aware trigger must be made for each intercept
%%   function.
%%
%% * The intercept-arg-aware trigger gives the name of the intercept
%%   function via #fi.trigger_with_intercepts_args=Name::string()
%%
%% * The name of the intercept-arg-aware trigger that is created by
%%   trigger_args_of_intercept:config/14 is:
%%        "i_arg_" ++ name of intercept function ++ "_" ++ suffix string
%%
%% * The details of the implementation aren't fully parameterized by
%%   the config() func.  Details such as:
%%   * We're going to use strstr(3) to implement our trigger.  This
%%     found is inside TBody.
%%   * We need to store strstr()'s 2nd argument somehow, so the
%%     'strstr_2nd_arg' detail is found in TStruct, TNewInstanceArgs,
%%     TNewInstanceBody, and TBody.
%%   * TBody also needs strstr()'s 1st argument, which we want to be
%%     the 'path' argument for our intercept's unlink(2)/unlinkat(2) call.
%%     - The #fi.intercept_args_call embeds this info for us, i.e.
%%       - #fi.intercept_args_call="path" to represent
%%         unlink(path)
%%       - #fi.intercept_args_call="__fd, path, __flag" to represent
%%         unlinkat(__fd, path, __flag)
%%     - We intentionally using the same variable name, "path", for both
%%       intercept funcs, to be the argument that we wish to examine.

config() ->
    CHeaders = ["<string.h>"],
    TStruct = "
typedef struct {
    char *i_name; /* Must be present */
    char *strstr_2nd_arg;
}",
    TNewInstanceArgs = "char *strstr_2nd_arg",
    TNewInstanceBody = "
    a->strstr_2nd_arg = malloc(strlen(strstr_2nd_arg) + 1);
    strcpy(a->strstr_2nd_arg, strstr_2nd_arg);
",
    %% NOTE: 'path' below matches last arg to config()
    TBody = "
    if (strstr(path, a->strstr_2nd_arg) != NULL) {
        res = 1;
    } else {
        res = 0;
    }
",

    [trigger_args_of_intercept:config(
         CHeaders, [], [],
         true, true, false,
         TStruct, "", TNewInstanceArgs, TNewInstanceBody,
         InterceptName, TBody,
         InterceptName, "pathstrstr") || InterceptName <- ["unlink", "unlinkat"]]
    ++
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
         %% Use 2-tuple version here, have the instance name auto-generated
         intercept_triggers = [{"i_arg_unlink_pathstrstr", "\"foofoo\""}]
     },
     #fi{
         name = "unlinkat",	% Linux 3.2 version
         type = intercept,
         intercept_args = "int __fd, __const char *path, int __flag",
         intercept_args_call = "__fd, path, __flag",
         c_headers = ["<unistd.h>"],
         intercept_errno = "EREMOTE",
         intercept_return_type = "int",
         intercept_return_value = "-1",
         %% Use 2-tuple version here, have the instance name auto-generated
         intercept_triggers = [{"i_arg_unlinkat_pathstrstr", "\"foofoo\""}]
     }
    ].
