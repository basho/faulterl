%% License terms for this source file are identical to the ?LICENSE macro below.

-define(LICENSE, "
/*
-------------------------------------------------------------------

 fault injection primitives

 Copyright (c) 2014 Basho Technologies, Inc. All Rights Reserved.

 This file is provided to you under the Apache License,
 Version 2.0 (the \"License\"); you may not use this file
 except in compliance with the License.  You may obtain
 a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing,
 software distributed under the License is distributed on an
 \"AS IS\" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 KIND, either express or implied.  See the License for the
 specific language governing permissions and limitations
 under the License.

-------------------------------------------------------------------
*/

").

-module(faulterl).
-compile(export_all).

-include("faulterl.hrl").

make(OutFile) ->
    %% For dev/test/debugging purposes only
    make(OutFile,
         [#fi{
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
            },
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
             },
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
             },
          #fi{
               name = "unlink",
               type = intercept,
               intercept_args = "const char *path",
               c_headers = ["<unistd.h>"],
               intercept_errno = "EREMOTE",
               intercept_return_type = "int",
               intercept_return_value = "-1",
               intercept_triggers = [{"switchpanel", "switchpanel_42", "42"},
                                     {"random", "random_50", "50"}]
             }
          ]).

make(OutFile, C) when is_list(OutFile) ->
    {ok, OUT} = file:open(OutFile, [write]),
    try
        make(OUT, C)
    after
        file:close(OUT)
    end;
make(OUT, C) ->
    put(output_file, OUT),
    I_Hdrs = [{X#fi.name, X#fi.c_headers} || X <- C],
    _I_CDecl_Extra = [{X#fi.name, X#fi.c_decl_extra} || X <- C],
    make_push(global_syms),

    px(?LICENSE),

    [px("#include ~s\n", [Hdr]) ||
        Hdr <- ["<stdio.h>", "<stdlib.h>", "<stdarg.h>", "<dlfcn.h>",
                "<sys/types.h>", "<sys/errno.h>"]],

    [px("#include ~s\n", [Hdr]) || {_I, HdrList} <- I_Hdrs, Hdr <- HdrList],
    p(""),

    [begin
         p("u_int8_t  ~s = 1;", [X]),
         push(global_syms, X)
     end || X <- ["bc_fi_enabled", "bc_fi_verbose"]],
    p(""),

    [begin
         V1 = flat_format("bc_fi_~s_enabled", [Name]),
         p("u_int8_t  ~s = ~w;", [V1, bool_to_int(Enabled)]),
         push(global_syms, V1),
         V2 = flat_format("bc_fi_~s_disabled_trigger", [Name]),
         p("u_int8_t  ~s = ~w;", [V2, bool_to_int(TriggerDisabled)]),
         push(global_syms, V2),
         V3 = flat_format("bc_fi_~s_verbose", [Name]),
         p("u_int8_t  ~s = ~w;", [V3, bool_to_int(Verbose)]),
         push(global_syms, V3),
         [p(XTra) || XTra <- CDX],
         [push(global_syms, Sym) || Sym <- XtraGlobalSyms],
         p("")
     end || #fi{type=trigger, name=Name, enabled=Enabled,
                trigger_disabled=TriggerDisabled, verbose=Verbose,
                c_decl_extra=CDX, extra_global_syms=XtraGlobalSyms} <- C],

    [begin
         p("u_int8_t  bc_fi_~s_verbose = ~w;", [Name, bool_to_int(Verbose)]),
         if FErrno == 'undefined' ->
                 ok;
            true ->
                 V1 = flat_format("bc_fi_~s_fake_errno", [Name]),
                 p("u_int8_t  ~s = ~s;", [V1, errno_conversion(FErrno)]),
                 push(global_syms, V1),
                 V2 = flat_format("bc_fi_~s_fake_return", [Name]),
                 p("~s ~s = ~s;", [FReturnT, V2, FReturnVal]),
                 push(global_syms, V2)
         end,
         p("")
     end || #fi{type=intercept, name=Name, verbose=Verbose,
                intercept_errno=FErrno,
                intercept_return_type=FReturnT,
                intercept_return_value=FReturnVal} <- C],

    [begin
         p("~s bc_fi_~s_t;", [TS, Name]),
         p("")
     end || #fi{type=trigger, name=Name, trigger_struct=TS} <- C],

    [begin
         p("static void init_~s()", [Name]),
         p("{"),
         p(Init),
         p("}"),
         p("")
     end || #fi{type=trigger, name=Name, trigger_init=Init} <- C],

    p("static void init_all_once()"),
    p("{"),
    p("static int done = 0;"),
    p(""),
    p("    if (!done) {"),
    [p("        init_~s();", [Name]) || #fi{type=trigger, name=Name} <- C],
    p("        done = 1;"),
    p("    }"),
    p("}"),
    p(""),

    [begin
         p("static void new_~s_instance(bc_fi_~s_t *a, char *i_name, ~s)",
            [Name, Name, Args]),
         p("{"),
         p("    init_all_once();"),
         p("    a->i_name = malloc(strlen(i_name) + 1);"),
         p("    strcpy(a->i_name, i_name);"),
         px(Body),
         p("}"),
         p("")
     end || #fi{type=trigger, name=Name, trigger_new_instance_args=Args,
                trigger_new_instance_body=Body} <- C],

    [begin
         p("int trigger_~s(bc_fi_~s_t *a, char *intercept_name)", [Name, Name]),
         p("{"),
         p("    int res;"),
         p(""),
         p("    if (! bc_fi_~s_enabled) {", [Name]),
         p("        res = bc_fi_~s_disabled_trigger;", [Name]),
         p("        if (bc_fi_verbose || bc_fi_~s_verbose) {", [Name]),
         p("            fprintf(stderr, \"trigger_~s: not enabled, trigger=%d\\r\\n\", res);", [Name]),
         p("        }"),
         p("        return res;"),
         p("    }"),
         p("~s", [Body]),
         p("    if (bc_fi_verbose || bc_fi_~s_verbose) {", [Name]),
         p("        fprintf(stderr, \"trigger_~s: name=%s intercept=%s trigger=%d\\r\\n\",", [Name]),
         p("                a->i_name, intercept_name, res);"),
         p("    }"),
         p("    return res;"),
         p("}"),
         p("")
     end || #fi{type=trigger, name=Name, trigger_body=Body} <- C],

    [begin
         p("int ~s(~s)", [Name, Args]),
         p("{"),
         [p("    static bc_fi_~s_t *a_~s = NULL;", [TType, TInstance]) ||
             {TType, TInstance, _Args} <- Triggers],
         p("    static char *real_name = \"~s\";", [Name]),
         p("    static ~s (*real)();", [FReturnT]),
         p("    int trigger;"),
         p("    ~s res;", [FReturnT]),
         p(""),
         p("    if (bc_fi_enabled) {"),
         [begin
              p("        if (a_~s == NULL) {", [TInstance]),
              p("            char *i_name = \"~s\";", [TInstance]),
              p(""),
              p("            a_~s = malloc(sizeof(bc_fi_~s_t));", [TInstance, TType]),
              p("            new_~s_instance(a_~s, i_name, ~s);", [TType, TInstance, TArgs]),
              p("        }")
          end || {TType, TInstance, TArgs} <- Triggers],

         p("        if (real == NULL) {"),
         p("            if ((real = (int (*)()) dlsym(RTLD_NEXT, real_name)) == NULL) {"),
         p("                fprintf(stderr, \"Fatal error: %s\\r\\n\", dlerror());"),
         p("                abort();"),
         p("            }"),
         p("        }"),
         p("        trigger=1;"),
         [begin
              p("        if (trigger) {"),
              p("            trigger = trigger_~s(a_~s, real_name);", [TType, TInstance]),
              p("        }")
          end || {TType, TInstance, _TArgs} <- Triggers],
         p("    } else {"),
         p("        trigger = 0;"),
         p("    }"),
         p(""),
         p("    if (trigger) {"),
         if ReturnGeneric == undefined ->
                 p("        errno = bc_fi_~s_fake_errno;", [Name]),
                 p("        res = bc_fi_~s_fake_return;", [Name]);
            true ->
                 px(ReturnGeneric)
         end,
         p("    } else {"),
         p("        res = (*real)(path);"),
         p("    }"),
         p("    if (bc_fi_verbose || bc_fi_~s_verbose) {", [Name]),
         p("        fprintf(stderr, \"intercept: %s: return=%d\\r\\n\","),
         p("                real_name, res);"),
         p("    }"),
         p("    return res;"),
         p("}"),
         ok
     end || #fi{type=intercept, name=Name,
                intercept_args=Args, intercept_return_type=FReturnT,
                intercept_triggers=Triggers,
                intercept_return_generic=ReturnGeneric} <- C],
    p(""),

    [p("%%-export-%% ~s", [Sym]) || Sym <- lists:reverse(get(global_syms))],
    ok.

bool_to_int(true) ->
    1;
bool_to_int(false) ->
    0.

errno_conversion(undefined) ->
    "0";
errno_conversion(X) ->
    re:replace(X, "'", "", [global, {return, list}]).

px(Fmt) ->
    px(Fmt, []).

px(Fmt, Args) ->
    io:format(Fmt, Args),
    ok = io:format(get(output_file), Fmt, Args).

p(Fmt) ->
    px(Fmt ++ "\n").

p(Fmt, Args) ->
    px(Fmt ++ "\n", Args).

make_push(P) ->
    put(P, []).

push(P, Item) ->
    put(P, [Item|get(P)]).

flat_format(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).
