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
-module(app_view).

-include("leofs_doctor.hrl").

%% Module API
-export([draw/1]).

-define(LEN_LOADED_COL_1, 24).
-define(LEN_LOADED_COL_2, 10).
-define(LEN_LOADED_COL_3, 40).

-define(LEN_RUNNING_COL_1, 24).
-define(LEN_RUNNING_COL_2, 30).


%% =============================================================================
%% Module API
%% =============================================================================
draw(State) ->
    case rpc:call(State#state.node,
                  application, info, []) of
        {badrpc,_Cause} ->
            [];
        timeout ->
            [];
        RetL ->
            LoadedLibs = lists:sort(leo_misc:get_value(loaded, RetL)),
            RunningApps = lists:sort(leo_misc:get_value(running, RetL)),

            ?PRINT("- Loaded libraries~n"),
            ?PRINTF("~s ~s~s~n",
                    [string:right("Library Name", ?LEN_LOADED_COL_1),
                     string:left("Version", ?LEN_LOADED_COL_2),
                     string:left("Desctiption", ?LEN_LOADED_COL_3)]),
            ok = draw_1(loaded, LoadedLibs),

            ?PRINT("- Running apps~n~n"),
            ?PRINTF("~s ~s~n",
                    [string:right("App Name", ?LEN_RUNNING_COL_1),
                     string:left("Pid", ?LEN_RUNNING_COL_2)]),
            ok = draw_1(running, RunningApps)
    end,
    ok.


%% =============================================================================
%% Internal Functions
%% =============================================================================
%% @private
draw_1(_,[]) ->
    ?PRINT("~n"),
    ok;
draw_1(loaded = Type, [{LibName, Desc, Ver}|Rest]) ->
    ?PRINTF("~s ~s~s~n",
            [string:right(atom_to_list(LibName), ?LEN_LOADED_COL_1),
             string:left(Ver, ?LEN_LOADED_COL_2),
             string:left(Desc, ?LEN_LOADED_COL_3)]),
    draw_1(Type, Rest);
draw_1(running = Type, [{AppName, Pid}|Rest]) ->
    Pid_1 = case is_pid(Pid) of
                true ->
                    pid_to_list(Pid);
                false ->
                    atom_to_list(Pid)
            end,
    ?PRINTF("~s ~s~n",
            [string:right(atom_to_list(AppName), ?LEN_RUNNING_COL_1),
             string:left(Pid_1, ?LEN_RUNNING_COL_2)]),
    draw_1(Type, Rest).
