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
-module(entop_view).

-include("leofs_doctor.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Module API
-export([draw/1]).

%% =============================================================================
%% Module API
%% =============================================================================
draw(State) ->
    draw_1(0, State).

%% @private
draw_1(Times, #state{times = Times_1}) when Times == Times_1 ->
    ok;
draw_1(Times, #state{times = Times_1,
                     interval = Interval} = State) ->
    %% Output a number of times
    case Times of
        0 ->
            ?PRINT(io_lib:format("#~w:~n",[Times + 1]));
        _ ->
            ?PRINT(io_lib:format("~n#~w:~n",[Times + 1]))
    end,

    %% Retrieve an entop results,
                                                %$ then output them
    NState = load_remote_static_data(State),
    remote_load_code(NState#state.remote_module, State#state.node),
    NState2 = init_callback(NState),
    fetch_and_update(NState2),

    %% Wait for the next procedure
    case (Times_1 > 1) of
        true ->
            timer:sleep(timer:seconds(Interval));
        false ->
            void
    end,
    draw_1(Times + 1, State).


%% =============================================================================
%% Internal Functions
%% =============================================================================
load_remote_static_data(State) ->
    RPC = fun(M, F, A) ->
                  rpc:call(State#state.node, M, F, A)
          end,
    Otp = RPC(erlang, system_info, [otp_release]),
    Erts = RPC(erlang, system_info, [version]),
    {Os1, Os2} = RPC(os, type, []),
    OsVers = RPC(os, version, []),
    Flags = [{cpus, RPC(erlang, system_info, [logical_processors])},
	     {smp, RPC(erlang, system_info, [smp_support])},
	     {a_threads, RPC(erlang, system_info, [thread_pool_size])},
	     {kpoll, RPC(erlang, system_info, [kernel_poll])}],
    State#state{ otp_version = Otp,
                 erts_version = Erts,
		 os_fam = Os1,
                 os = Os2,
                 os_version = OsVers,
                 node_flags = Flags }.

remote_load_code(Module, Node) ->
    {_, Binary, Filename} = code:get_object_code(Module),
    rpc:call(Node, code, load_binary, [Module, Filename, Binary]).

init_callback(#state{callback = CB, node = Node} = State) ->
    {ok, Columns, CBState} = CB:init(Node),
    State#state{ columns = Columns, cbstate = CBState }.

print_nodeinfo(State) ->
    {Mj, Md, Mi} = State#state.os_version,
    OsVers = lists:concat([Mj,".",Md,".",Mi]),
    ?PRINT(io_lib:format("Node: ~p ~n",[State#state.node])),
    Head = io_lib:format(" (~s/~s) ~p (~p ~s)~s~n",
			 [State#state.otp_version,
			  State#state.erts_version, State#state.os_fam,
			  State#state.os, OsVers, flags2str(State#state.node_flags)]),
    ok = ?PRINT(lists:flatten(Head)).

flags2str([]) -> [];
flags2str([{cpus, N}|Rest]) ->
    [" CPU:"++integer_to_list(N)|flags2str(Rest)];
flags2str([{smp, true}|Rest]) ->
    [" SMP"|flags2str(Rest)];
flags2str([{a_threads, N}|Rest]) ->
    [" +A:"++integer_to_list(N)|flags2str(Rest)];
flags2str([{kpoll, true}|Rest]) ->
    [" +K"|flags2str(Rest)];
flags2str([_|Rest]) ->
    flags2str(Rest).

fetch_data(Node, Module) ->
    timer:tc(rpc, call, [Node, Module, get_data, []]).

fetch_and_update(State) ->
    case fetch_data(State#state.node, State#state.remote_module) of
	{_Time, {badrpc, _}} ->
	    {error, badrpc};
	{Time, {ok, HeaderData, RowDataList}} ->
	    update_screen(Time, HeaderData, RowDataList, State)
    end.

update_screen(Time, HeaderData, RowDataList, State) ->
    print_nodeinfo(State),
    print_showinfo(State, Time),
    {Headers, State1} = process_header_data(HeaderData, State),
    lists:foreach(fun(Header) ->
                          %% cecho:hline($ , ?MAX_HLINE),
                          ?PRINT(Header)
                  end, Headers),
    draw_title_bar(State#state.columns, ""),
    {RowList, State2} = process_row_data(RowDataList, State1),
    SortedRowList = sort(RowList, State),
    update_rows(SortedRowList, State2#state.columns,
                0, State2#state.topn).

draw_title_bar([], Acc) ->
    ?PRINT(Acc),
    ?PRINT("~n"),
    ok;
draw_title_bar([{Title, Width, Options}|Rest], Acc) ->
    Align = proplists:get_value(align, Options, left),
    NAcc = Acc ++ string:Align(Title, Width) ++ " ",
    draw_title_bar(Rest, NAcc).

print_showinfo(State, RoundTripTime) ->
    ColName = element(1,lists:nth(State#state.sort, State#state.columns)),
    SortName = if State#state.reverse_sort -> "Descending"; true -> "Ascending" end,
    Showing = io_lib:format("Sorting on ~p (~s), Retrieved in ~pms~n",
			    [ColName, SortName, RoundTripTime div 1000]),
    ?PRINT(lists:flatten(Showing)).

process_header_data(HeaderData, State) ->
    {ok, Headers, NCBState} =
        (State#state.callback):header(HeaderData, State#state.cbstate),
    {Headers, State#state{ cbstate = NCBState }}.

process_row_data(RowDataList, State) ->
    prd(RowDataList, State, []).

prd([], State, Acc) ->
    {Acc, State};
prd([RowData|Rest], State, Acc) ->
    case (State#state.callback):row(RowData, State#state.cbstate) of
	{ok, skip, NCBState} ->
	    prd(Rest, State#state{ cbstate = NCBState }, Acc);
	{ok, Row, NCBState} ->
	    prd(Rest, State#state{ cbstate = NCBState }, [Row|Acc])
    end.

sort(ProcList, State) ->
    Sorted = lists:keysort(State#state.sort, ProcList),
    case State#state.reverse_sort of
	true ->
	    lists:reverse(Sorted);
	false ->
	    Sorted
    end.

update_rows(ProcValuesList, _, LineNumber, Max)
  when LineNumber == Max orelse ProcValuesList == [] ->
    ok;
update_rows([RowValues|Rest], Columns, LineNumber, Max) ->
    update_row(tuple_to_list(RowValues), Columns, LineNumber, ""),
    update_rows(Rest, Columns, LineNumber + 1, Max).

update_row(R, C, _, Acc) when R == [] orelse C == [] ->
    ?PRINT(Acc),
    ?PRINT("~n"),
    ok;
update_row([RowColValue|Rest], [{_,Width,Options}|RestColumns], LineNumber, Acc) ->
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
    update_row(Rest, RestColumns,
               LineNumber, Acc ++ Aligned ++ " ").
