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
%%======================================================================
-module(entop_format).

%% Module API
-export([init/1, header/2, row/2]).
-export([colname_to_idx/1]).

%% Records
-record(state, { node = undefined, cache = [] }).

%% Defines
-define(KIB,(1024)).
-define(MIB,(?KIB*1024)).
-define(GIB,(?MIB*1024)).
-define(SECONDS_PER_MIN, 60).
-define(SECONDS_PER_HOUR, (?SECONDS_PER_MIN*60)).
-define(SECONDS_PER_DAY, (?SECONDS_PER_HOUR*24)).
-define(R(V,N), string:right(integer_to_list(V),N,$0)).

%% =============================================================================
%% Module API
%% =============================================================================
colname_to_idx("reductions") ->
    5;
colname_to_idx("red") ->
    5;
colname_to_idx("mqueue") ->
    6;
colname_to_idx("mq") ->
    6;
colname_to_idx("hsize") ->
    7;
colname_to_idx("hs") ->
    7;
colname_to_idx("ssize") ->
    8;
colname_to_idx("ss") ->
    8;
colname_to_idx("htotal") ->
    9;
colname_to_idx("ht") ->
    9.

init(Node) ->
    Columns = [{"Pid", 16, [{align, right}]},
               {"Registered Name", 20, []},
               {"Initial Call", 30, []},
               {"Current Function", 40, []},
	       {"Reductions", 12, []},
	       {"MQueue", 6, []},
	       {"HSize", 12, []},
	       {"SSize", 6, []},
	       {"HTotal", 12, []}],
    {ok, Columns, #state{ node = Node }}.

%% Header Callback
header(SystemInfo, State) ->
    Uptime = millis2uptimestr(element(1, proplists:get_value(uptime, SystemInfo, 0))),
    LocalTime = local2str(element(2, proplists:get_value(local_time, SystemInfo))),
    PingTime = element(1,timer:tc(net_adm, ping, [State#state.node])) div 1000,
    Row1 = io_lib:format("Time: local time ~s, up for ~s, ~pms latency, ~n",
			 [LocalTime, Uptime, PingTime]),

    PTotal = proplists:get_value(process_count, SystemInfo),
    RQueue = proplists:get_value(run_queue, SystemInfo),
    RedTotal = element(2,proplists:get_value(reduction_count, SystemInfo)),
    PMemUsed = proplists:get_value(process_memory_used, SystemInfo),
    PMemTotal = proplists:get_value(process_memory_total, SystemInfo),
    Row2 = io_lib:format("Processes: total ~p (RQ ~p) at ~p RpI using ~s (~s allocated)~n",
			 [PTotal, RQueue, RedTotal, mem2str(PMemUsed), mem2str(PMemTotal)]),

    MemInfo = proplists:get_value(memory, SystemInfo),
    SystemMem = mem2str(proplists:get_value(system, MemInfo)),
    AtomMem = mem2str(proplists:get_value(atom, MemInfo)),
    AtomUsedMem = mem2str(proplists:get_value(atom_used, MemInfo)),
    BinMem = mem2str(proplists:get_value(binary, MemInfo)),
    CodeMem = mem2str(proplists:get_value(code, MemInfo)),
    EtsMem = mem2str(proplists:get_value(ets, MemInfo)),
    Row3 = io_lib:format("Memory: Sys ~s, Atom ~s/~s, Bin ~s, Code ~s, Ets ~s~n",
			 [SystemMem, AtomUsedMem, AtomMem, BinMem, CodeMem, EtsMem]),
    Row4 = "~n",
    {ok, [ lists:flatten(Row) || Row <- [Row1, Row2, Row3, Row4] ], State}.

%% Column Specific Callbacks
row([{pid,_}|undefined], State) ->
    {ok, skip, State};
row(ProcessInfo, State) ->
    Pid = proplists:get_value(pid, ProcessInfo),
    RegName = case proplists:get_value(registered_name, ProcessInfo) of
		  [] ->
		      "-";
		  Name ->
		      atom_to_list(Name)
	      end,
    InitialCall = fun2str(proplists:get_value(initial_call, ProcessInfo, 0)),
    CurrentFunction = fun2str(proplists:get_value(current_function, ProcessInfo, 0)),
    Reductions = proplists:get_value(reductions, ProcessInfo, 0),
    Queue = proplists:get_value(message_queue_len, ProcessInfo, 0),
    Heap = proplists:get_value(heap_size, ProcessInfo, 0),
    Stack = proplists:get_value(stack_size, ProcessInfo, 0),
    HeapTot = proplists:get_value(total_heap_size, ProcessInfo, 0),
    {ok, {Pid, RegName, InitialCall, CurrentFunction, Reductions, Queue, Heap, Stack, HeapTot}, State}.

fun2str({M, N, A}) ->
    lists:append([atom_to_list(M), ":", atom_to_list(N), "/", integer_to_list(A)]).

mem2str(Mem) ->
    if Mem > ?GIB -> io_lib:format("~.1fm",[Mem/?MIB]);
       Mem > ?KIB -> io_lib:format("~.1fk",[Mem/?KIB]);
       Mem >= 0 -> io_lib:format("~.1fb",[Mem/1.0])
    end.

millis2uptimestr(Millis) ->
    SecTime = Millis div 1000,
    Days = ?R(SecTime div ?SECONDS_PER_DAY,3),
    Hours = ?R((SecTime rem ?SECONDS_PER_DAY) div ?SECONDS_PER_HOUR,2),
    Minutes = ?R(((SecTime rem ?SECONDS_PER_DAY) rem ?SECONDS_PER_HOUR) div ?SECONDS_PER_MIN, 2),
    Seconds = ?R(((SecTime rem ?SECONDS_PER_DAY) rem ?SECONDS_PER_HOUR) rem ?SECONDS_PER_MIN, 2),
    io_lib:format("~s:~s:~s:~s",[Days,Hours,Minutes,Seconds]).

local2str({Hours,Minutes,Seconds}) ->
    io_lib:format("~s:~s:~s",[?R(Hours,2),?R(Minutes,2),?R(Seconds,2)]).
