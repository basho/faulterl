%% -------------------------------------------------------------------
%%
%%  fault injection primitives
%%
%%  Copyright (c) 2014 Basho Technologies, Inc. All Rights Reserved.
%%
%%  This file is provided to you under the Apache License,
%%  Version 2.0 (the "License"); you may not use this file
%%  except in compliance with the License.  You may obtain
%%  a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%%  Unless required by applicable law or agreed to in writing,
%%  software distributed under the License is distributed on an
%%  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%  KIND, either express or implied.  See the License for the
%%  specific language governing permissions and limitations
%%  under the License.
%%
%% -------------------------------------------------------------------

-module(faulterl_util).

-export([disterl_ports/0, disterl_port/1, disterl_fd_num/1]).

disterl_ports() ->
    {ok, L} = net_kernel:nodes_info(),
    lists:flatten(
      [try
         Owner = proplists:get_value(owner, Ps),
         {links, Ls} = process_info(Owner, links),
         [DistErlPort] = [P || P <- Ls, is_port(P)],
         {ok, FdNum} = inet:getfd(DistErlPort),
         {Node, {DistErlPort, FdNum}}
       catch _:_ ->
               []
       end || {Node, Ps} <- L]).

disterl_port(Node) ->
    {Port, _FdNum} = proplists:get_value(Node, disterl_ports()),
    Port.

disterl_fd_num(Node) ->
    {_Port, FdNum} = proplists:get_value(Node, disterl_ports()),
    FdNum.
