%%====================================================================
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
%% -------------------------------------------------------------------
%% LeoFS' Doctor
%% @doc
%% @end
%%====================================================================
-module(leofs_doctor_sup_tree).

-include_lib("eunit/include/eunit.hrl").

-export([info/2, print/2, write/3,
         compare/3
        ]).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Retrieve a supervisor tree
-spec(info(Node, SupRef) ->
             ok | {error, Cause} when Node::atom(),
                                      SupRef::supervisor:sup_ref(),
                                      Cause::any()).
info(Node, SupRef) ->
    info_1(Node, SupRef, 0, []).

%% @private
info_1(Node, SupRef, Level, Acc) ->
    case catch rpc:call(Node, supervisor, which_children, [SupRef]) of
        {'EXIT', Cause} ->
            {error, Cause};
        RetL ->
            info_2(RetL, Node, Level + 1, Acc)
    end.

%% @private
info_2([],_Node,_Level, Acc) ->
    {ok, Acc};
info_2([{Id, Pid, supervisor = Type,_Modules}|Rest], Node, Level, Acc) ->
    Acc_1 = Acc ++ [{Id, pid_to_list(Pid), Type, Level}],
    {ok, Acc_2} = info_1(Node, Id, Level, Acc_1),
    info_2(Rest, Node, Level, Acc_2);

info_2([{Id, Pid, Type,_Modules}|Rest], Node, Level, Acc)->
    Acc_1 = Acc ++ [{Id, pid_to_list(Pid), Type, Level}],
    info_2(Rest, Node, Level, Acc_1).


%% @doc Print a supervisor's tree
-spec(print(Node, SupRef) ->
             ok | {error, Cause} when Node::atom(),
                                      SupRef::supervisor:sup_ref(),
                                      Cause::any()).
print(Node, SupRef) ->
    case info(Node, SupRef) of
        {ok, RetL} ->
            print_1(RetL);
        Error ->
            Error
    end.

%% @private
print_1([]) ->
    ok;
print_1([{Id, Pid, Type, Level}|Rest]) ->
    io:format("~s~p | ~p | ~p~n",
              [lists:duplicate((Level * 4), $ ), Id, Pid, Type]),
    print_1(Rest).


%% @doc Write a supervisor tree in the file
-spec(write(Node, SupRef, FileName) ->
             ok | {error, Cause} when Node::atom(),
                                      SupRef::supervisor:sup_ref(),
                                      FileName::string(),
                                      Cause::any()).
write(Node, SupRef, FileName) ->
    case info(Node, SupRef) of
        {ok, RetL} ->
            leo_file:file_unconsult(FileName, RetL);
        Error ->
            Error
    end.


%% @doc Compare a current supervisor-tree with a dumped supervisor-tree
-spec(compare(Node, SupRef, DumpSupFile) ->
             {ok, [term()]} | {error, Cause} when Node::atom(),
                                                  SupRef::supervisor:sup_ref(),
                                                  DumpSupFile::string(),
                                                  Cause::any()).
compare(Node, SupRef, DumpSupFile) ->
    case filelib:is_file(DumpSupFile) of
        true ->
            case file:consult(DumpSupFile) of
                {ok, RetLPrev} ->
                    case info(Node, SupRef) of
                        {ok, RetLCur} ->
                            {ok, Acc_1} = compare_1(RetLCur, RetLPrev, []),
                            compare_2(RetLPrev, RetLCur, Acc_1);
                        Error ->
                            Error
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, []}
    end.

%% @private
compare_1([],_,Acc) ->
    {ok, lists:reverse(Acc)};
compare_1([H|Rest], RetLPrev, Acc) ->
    {Id,_,Type,Level} = H,
    Acc_1 = case lists:keyfind(Id, 1, RetLPrev) of
                false ->
                    [{added, {Id, Type, Level}}|Acc];
                {Id,_,Type_1,Level_1} ->
                    case (Type == Type_1 andalso
                          Level == Level_1) of
                        true ->
                            Acc;
                        false ->
                            [{different_type_or_level, {Id, Type, Level}}|Acc]
                    end
            end,
    compare_1(Rest, RetLPrev, Acc_1).

%% @private
compare_2([],_,Acc) ->
    {ok, lists:reverse(Acc)};
compare_2([H|Rest], RetLCur, Acc) ->
    {Id,_,Type,Level} = H,
    Acc_1 = case lists:keyfind(Id, 1, RetLCur) of
                false ->
                    [{not_found, {Id, Type, Level}}|Acc];
                _ ->
                    Acc
            end,
    compare_2(Rest, RetLCur, Acc_1).
