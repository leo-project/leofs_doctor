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
%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(leofs_doctor).

-author('mazen.harake@erlang-solutions.com').

-behaviour(application).

-include("leofs_doctor.hrl").

%% Application Callbacks
-export([start/2, stop/1]).

%% Application API
-export([start/1]).

%% =============================================================================
%% Behaviour Callbacks
%% =============================================================================
start(_, _) ->
    Res = leofs_doctor_sup:start_link(),
    %% parameters related to connections
    Node = get_argument(target_node, 'manager_0@127.0.0.1', fun erlang:list_to_atom/1),
    %% parameters related to entop
    SortCol = get_argument(sort_col, 3, fun entop_format:colname_to_idx/1),
    Reverse = get_argument(reverse, true, fun list_to_bool/1),
    TopN = get_argument(topn, 10, fun erlang:list_to_integer/1),
    State = #state{ node = Node,
                    sort = SortCol,
                    reverse_sort = Reverse,
                    topn = TopN
                  },
    start(State),
    Res.

stop(_) ->
    ok.

%% =============================================================================
%% Application API
%% =============================================================================
start(#state{node = Node} = State) ->
    case net_kernel:connect(Node) of
        true ->
            entop_view:draw(State);
        false ->
            nop
    end,
    halt().

%% =============================================================================
%% Inner function
%% =============================================================================
get_argument(Key, Def, Modifier) ->
    case init:get_argument(Key) of
        {ok, [[Val]]} ->
            Modifier(Val);
        _ ->
            Def
    end.

list_to_bool("true") ->
    true;
list_to_bool("yes") ->
    true;
list_to_bool("y") ->
    true;
list_to_bool("false") ->
    false;
list_to_bool("no") ->
    false;
list_to_bool("n") ->
    false.
