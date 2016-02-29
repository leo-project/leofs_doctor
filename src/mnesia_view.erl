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
-module(mnesia_view).

-include("leofs_doctor.hrl").

%% Module API
-export([draw/1]).

%% =============================================================================
%% Module API
%% =============================================================================
draw(State) ->
    remote_load_code(mnesia_collector, State#state.node),
    fetch_and_update(State).

%% =============================================================================
%% Internal Functions
%% =============================================================================
remote_load_code(Module, Node) ->
    {_, Binary, Filename} = code:get_object_code(Module),
    rpc:call(Node, code, load_binary, [Module, Filename, Binary]).

fetch_and_update(State) ->
    case rpc:call(State#state.node, mnesia_collector, get_data, []) of
        {badrpc, _} ->
    	    {error, badrpc};
        {ok, SysInfo, TableProplist} ->
            update_screen(SysInfo, TableProplist)
    end.

update_screen(SysInfo, TableProplist) ->
    print_sysinfo(SysInfo),
    print_tableinfo(TableProplist).

cols() ->
    [{"Table Name", 25, [{align, right}]},
     {"Access Mode", 12, []},
     {"Memory", 10, []},
     {"Size", 10, []}].

row(TableInfo) ->
    Name= proplists:get_value(table_name, TableInfo),
    Access = proplists:get_value(access_mode, TableInfo, read_write),
    Memory = proplists:get_value(memory, TableInfo, 0),
    Size = proplists:get_value(size, TableInfo, 0),
    {ok, {Name, Access, Memory, Size}}.

prd([], Acc) ->
    Acc;
prd([RowData|Rest], Acc) ->
    case row(RowData) of
        {ok, Row} ->
            prd(Rest, [Row|Acc]);
        _ ->
            prd(Rest, Acc)
    end.

draw_title_bar([], Acc) ->
    ?PRINT(Acc),
    ?PRINT("~n"),
    ok;
draw_title_bar([{Title, Width, Options}|Rest], Acc) ->
    Align = proplists:get_value(align, Options, left),
    NAcc = Acc ++ string:Align(Title, Width) ++ " ",
    draw_title_bar(Rest, NAcc).

print_sysinfo(SysInfo) ->
    Version = proplists:get_value(version, SysInfo, "Unknown"),
    IsRunning = proplists:get_value(is_running, SysInfo, yes),
    LockQueue = proplists:get_value(lock_queue, SysInfo, []),
    Trans = proplists:get_value(transactions, SysInfo, []),
    TranFailures = proplists:get_value(transaction_failures, SysInfo, 0),
    TranCommits = proplists:get_value(transaction_commits, SysInfo, 0),
    TranRestarts = proplists:get_value(transaction_restarts, SysInfo, 0),
    TranLogWrites = proplists:get_value(transaction_log_writes, SysInfo, 0),
    Format = lists:append([
                           "- System Info~n",
                           "\t- version: ~p~n",
                           "\t- is_running: ~p~n",
                           "\t- lock_queue: ~p~n",
                           "\t- transactions: ~p~n",
                           "\t- transaction_failures: ~p~n",
                           "\t- transaction_commits: ~p~n",
                           "\t- transaction_restarts: ~p~n",
                           "\t- transaction_log_writes: ~p~n"
                          ]),
    ?PRINTF(Format, [Version,
                     IsRunning,
                     LockQueue,
                     Trans,
                     TranFailures,
                     TranCommits,
                     TranRestarts,
                     TranLogWrites]).

print_tableinfo(TableProplist) ->
    ?PRINT("~n- Table Info~n~n"),
    Cols = cols(),
    draw_title_bar(Cols, ""),
    RowList = prd(TableProplist, []),
    update_rows(RowList, Cols).

update_rows([], _) ->
    ok;
update_rows([RowValues|Rest], Columns) ->
    update_row(tuple_to_list(RowValues), Columns, ""),
    update_rows(Rest, Columns).

update_row(R, C, Acc) when R == [] orelse C == [] ->
    ?PRINT(Acc),
    ?PRINT("~n"),
    ok;
update_row([RowColValue|Rest], [{_,Width,Options}|RestColumns], Acc) ->
    StrColVal = if is_list(RowColValue) ->
			RowColValue;
		   true ->
			lists:flatten(io_lib:format("~1000p",[RowColValue]))
		end,
    Aligned = case proplists:get_value(align, Options) of
		  right ->
		      string:right(StrColVal, Width);
		  _ ->
		      string:left(StrColVal, Width)
	      end,
    update_row(Rest, RestColumns, Acc ++ Aligned ++ " ").
