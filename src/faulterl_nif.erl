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
-module(faulterl_nif).

-export([init/0,
         peek/5,
         poke/4]).

-on_load(init/0).

-ifdef(TEST).
-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-endif.
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    case code:priv_dir(faulterl) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    SoName = filename:join([filename:dirname(Filename),"../priv", "faulterl"]);
                _ ->
                    SoName = filename:join("../priv", "faulterl")
            end;
         Dir ->
                        SoName = filename:join(Dir, "faulterl")
    end,
    erlang:load_nif(SoName, 0).

peek(Key, Index, 1=Size, true=_StringP, DerefP) ->
    peek_int(Key, Index, Size, 1, bool_to_int(DerefP));
peek(Key, Index, Size, false, DerefP) ->
    peek_int(Key, Index, Size, 0, bool_to_int(DerefP)).

peek_int(_Key, _Index, _Size, _StringPInt, _DerefPInt) ->
    erlang:nif_error({error, not_loaded}).

poke(Key, Index, Val, DerefP) when is_list(Val); is_binary(Val) ->
    poke_int(Key, Index, Val, bool_to_int(DerefP)).

poke_int(_Key, _Index, _Val, _DerefPInt) ->
    erlang:nif_error({error, not_loaded}).

bool_to_int(true) ->
    1;
bool_to_int(false) ->
    0.

%% ===================================================================
%% EUnit tests
%% ===================================================================

-ifdef(TEST).

poke_deref_true_test() ->
    Const1 = 16#deadbeeffeedface,
    Const2 = 16#0102030405060708,

    {ok, <<0:64/native>>} = peek("poke_test_scratch", 0, 64, false, true),
    ok = poke("poke_test_scratch", 0, <<Const1:64/native>>, true),
    {ok, <<Const1:64/native>>} = peek("poke_test_scratch", 0, 64, false, true),

    ok = poke("poke_test_scratch", 7, <<16#01:8/native>>, true),
    ok = poke("poke_test_scratch", 6, <<16#02:8/native>>, true),
    ok = poke("poke_test_scratch", 5, <<16#03:8/native>>, true),
    ok = poke("poke_test_scratch", 4, <<16#04:8/native>>, true),
    ok = poke("poke_test_scratch", 3, <<16#05:8/native>>, true),
    ok = poke("poke_test_scratch", 2, <<16#06:8/native>>, true),
    ok = poke("poke_test_scratch", 1, <<16#07:8/native>>, true),
    ok = poke("poke_test_scratch", 0, <<16#08:8/native>>, true),
    {ok, <<Const2:64/native>>} = peek("poke_test_scratch", 0, 64, false, true),

    ok = poke("poke_test_scratch", 1, <<16#09000009:32/native>>, true),
    {ok, <<16#09000009:32/native>>} = peek("poke_test_scratch", 1, 32, false, true),
    ok = poke("poke_test_scratch", 0, <<16#00080800:32/native>>, true),
    {ok, <<16#00080800:32/native>>} = peek("poke_test_scratch", 0, 32, false, true),

    String = "The quick brown fox, and all that jazz.",
    ok = poke("poke_test_scratch", 42, String, false),
    {ok, String} = peek("poke_test_scratch", 42, 1, true, false),

    ok.

poke_deref_false_test() ->
    [begin
         {ok, <<Orig:Bitsize/native>>} = peek(Name, 0, Bitsize, false, false),
         ok = poke(Name, 0, <<New:Bitsize/native>>, false),
         {ok, <<New:Bitsize/native>>} = peek(Name, 0, Bitsize, false, false),
         ok = poke(Name, 0, <<Orig:Bitsize/native>>, false),
         {ok, <<Orig:Bitsize/native>>} = peek(Name, 0, Bitsize, false, false)
     end || {Name, Bitsize, Orig, New} <-
                [{"peek_test_int8", 8, 42, 253},
                 {"peek_test_int32", 32, 3242, 16#a0b0c0d0},
                 {"peek_test_int64", 64, 6442, 16#a0b0c0d001020304}]],

    ok.

peek_test() ->
    {ok, <<42:8/native>>} = peek("peek_test_int8", 0, 8, false, false),
    {ok, <<3242:32/native>>} = peek("peek_test_int32", 0, 32, false, false),
    {ok, <<6442:64/native>>} = peek("peek_test_int64", 0, 64, false, false),

    {ok, "It is 42, yes."} = peek("peek_test_string", 0, 1, true, false),
    {ok, "t is 42, yes."} = peek("peek_test_string", 1, 1, true, false),
    {ok, " is 42, yes."} = peek("peek_test_string", 2, 1, true, false),

    {ok, <<42:8/native>>} = peek("peek_test_intarray8", 0, 8, false, false),
    {ok, <<43:8/native>>} = peek("peek_test_intarray8", 1, 8, false, false),
    {ok, <<44:8/native>>} = peek("peek_test_intarray8", 2, 8, false, false),
    {ok, <<4242:32/native>>} = peek("peek_test_intarray32", 0, 32, false, false),
    {ok, <<4343:32/native>>} = peek("peek_test_intarray32", 1, 32, false, false),
    {ok, <<4444:32/native>>} = peek("peek_test_intarray32", 2, 32, false, false),
    {ok, <<424242:64/native>>} = peek("peek_test_intarray64", 0, 64, false, false),
    {ok, <<434343:64/native>>} = peek("peek_test_intarray64", 1, 64, false, false),
    {ok, <<444444:64/native>>} = peek("peek_test_intarray64", 2, 64, false, false),
    ok.

-endif.
