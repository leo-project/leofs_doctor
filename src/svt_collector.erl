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
-module(svt_collector).

%% Module API
-export([get_data/1]).

%% =============================================================================
%% Module API
%% =============================================================================
get_data(RootSup) ->
    SVT = build_svt(RootSup),
    SVT.

%% =============================================================================
%% Inner functions 
%% =============================================================================
build_svt(RootSup) ->
    build_svt(RootSup, []).

build_svt(Sup, Acc) ->
    Ret = supervisor:which_children(Sup),
    build_svt_1(Ret, Acc).

%% @private
build_svt_1([], Acc) ->
    {ok, Acc};
build_svt_1([{Id, _Pid, supervisor,_Modules}|Rest], Acc) ->
    {ok, NAcc} = build_svt(Id, []),
    build_svt_1(Rest, [{Id, NAcc}|Acc]);
build_svt_1([{Id, _Pid, _Type,_Modules}|Rest], Acc)->
    build_svt_1(Rest, [{Id, []}|Acc]).
