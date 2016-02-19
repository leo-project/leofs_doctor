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
    State = try
        %% show usage
        case get_argument(help, false, fun any_to_true/1) of
            true ->
                usage(),
                halt();
            false ->
                nop
        end,
        %% parameters related to connections
        Node = get_argument(target_node, 'manager_0@127.0.0.1', fun erlang:list_to_atom/1),
        %% parameters related to entop
        SortCol = get_argument(sort_col, 5, fun entop_format:colname_to_idx/1),
        Reverse = get_argument(reverse, true, fun list_to_bool/1),
        TopN = get_argument(topn, 10, fun erlang:list_to_integer/1),
        %% parameters related to supervisor tree
        RootSup = get_argument(root_sup, nop, fun erlang:list_to_atom/1),
        ExpectedSVT = get_argument(expected_svt, "", fun nop/1),
        #state{ node = Node,
                sort = SortCol,
                reverse_sort = Reverse,
                topn = TopN,
                root_sup = RootSup,
                expected_svt = ExpectedSVT
               }
    catch
        _:_ ->
            %% argument parse error occured
            usage(),
            halt()
    end,
    start(State),
    halt(),
    Res.

stop(_) ->
    ok.

%% =============================================================================
%% Application API
%% =============================================================================
start(#state{node = Node} = State) ->
    case net_kernel:connect(Node) of
        true ->
            {{Y,M,D},{H,MI,S}} = calendar:local_time(),
            ?PRINTF("Date: ~4w/~2..0w/~2..0w ~2..0w:~2..0w:~2..0w~n~n", [Y,M,D,H,MI,S]),
            ?PRINT("[entop]~n"),
            entop_view:draw(State),
            ?PRINT("~n[mnesia]~n"),
            mnesia_view:draw(State),
            ?PRINT("~n[supervisor tree]~n"),
            svt_view:draw(State);
        false ->
            ?PRINTF("Failed to connect ~p~n", [Node]),
            nop
    end.

%% =============================================================================
%% Inner function
%% =============================================================================
usage() ->
    Usage = lists:append([
                "Usage: leofs_doctor~n",
                "\t[-target_node <TARGET_NODE>]~n",
                "\t[-sort_col <COL_NAME>] [-reverse <yes|no>] [-topn <TOPN>]~n",
                "\t[-bin_leak <TOPN>] [-proc_count <ATTR_NAME,TOPN>] [-inet_count]~n",
                "\t[-system_info] [-table_info <TABLES>]~n",
                "\t[-expected_svt <FILENAME>]~n"
                ]),
    ?PRINT(Usage).

get_argument(Key, Def, Modifier) ->
    case init:get_argument(Key) of
        {ok, [[Val]]} ->
            Modifier(Val);
        _ ->
            Def
    end.

nop(S) ->
    S.

any_to_true(_) ->
    true.

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
