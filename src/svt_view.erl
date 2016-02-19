%%======================================================================
%%
%% LeoFS Doctor
%%
%% Copyright (c) 2012-2016 Rakuten, Inc.
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
%%======================================================================
-module(svt_view).

-include("leofs_doctor.hrl").

%% Module API
-export([draw/1]).

%% =============================================================================
%% Module API
%% =============================================================================
draw(State) ->
    remote_load_code(svt_collector, State#state.node),
    fetch_and_update(State).

%% =============================================================================
%% Internal Functions
%% =============================================================================
remote_load_code(Module, Node) ->
    {_, Binary, Filename} = code:get_object_code(Module),
    rpc:call(Node, code, load_binary, [Module, Filename, Binary]).

fetch_and_update(#state{root_sup = RootSup, expected_svt = Expected} = State) ->
    case rpc:call(State#state.node, svt_collector, get_data, [RootSup]) of
        {badrpc, _} ->
    	    {error, badrpc};
        {ok, SVT} ->
            update_screen(SVT, Expected)
    end.

update_screen(SVT, Expected) ->
    print_svt(SVT),
    check_svt(SVT, Expected).

print_svt(SVT) ->
    ?PRINTF("- current: ~p~n", [SVT]).

check_svt(_SVT, "") ->
    ok;
check_svt(CurrentSVT, Expected) ->
    [ExpectedSVT]= yamerl_constr:file(Expected),
    ?PRINTF("- expected: ~p~n", [ExpectedSVT]),
    Missing = ExpectedSVT -- CurrentSVT,
    ?PRINTF("- missing: ~p~n", [Missing]),
    ok.
