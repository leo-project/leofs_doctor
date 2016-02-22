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

-include("leofs_doctor.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([info/1, info/2,
         write/2, write/3
        ]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Retrieve a supervisor tree
-spec(info(SupRef) ->
             {ok, RetL} | {error, Cause} when SupRef::supervisor:sup_ref(),
                                              RetL::[#sup_tree{}],
                                              Cause::any()).
info(SupRef) ->
    info(erlang:node(), SupRef).

-spec(info(Node, SupRef) ->
             {ok, [#sup_tree{}]} | {error, Cause} when Node::atom(),
                                                       SupRef::supervisor:sup_ref(),
                                                       Cause::any()).
info(undefined, SupRef) ->
    info(SupRef);
info(Node, SupRef) ->
    case rpc:call(Node, erlang, whereis, [SupRef]) of
        undefined ->
            {error, undefined};
        {badrpc, Cause} ->
            {error, Cause};
        timeout = Cause ->
            {error, Cause};
        _Pid ->
            case catch rpc:call(
                         Node, supervisor, which_children, [SupRef]) of
                {badrpc, Cause} ->
                    {error, Cause};
                timeout = Cause ->
                    {error, Cause};
                RetL ->
                    info_1(RetL, Node, [])
            end
    end.

%% @private
info_1([],_,Acc) ->
    {ok, lists:reverse(Acc)};
info_1([{Id, Pid, supervisor = Type,_Modules}|Rest], Node, Acc) ->
    Children = case info(Node, Id) of
                   {ok, RetL} ->
                       RetL;
                   {error,_} ->
                       []
               end,
    info_1(Rest, Node, [#sup_tree{id = Id,
                                  pid = Pid,
                                  type = Type,
                                  children = Children}|Acc]);
info_1([{Id, Pid, Type,_Modules}|Rest], Node, Acc) ->
    info_1(Rest, Node, [#sup_tree{id = Id,
                                  pid = Pid,
                                  type = Type}|Acc]).

%% @doc Write a supervisor tree in the file
-spec(write(SupRef, FileName) ->
             ok | {error, Cause} when SupRef::supervisor:sup_ref(),
                                      FileName::string(),
                                      Cause::any()).
write(SupRef, FileName) ->
    Ret = info(SupRef),
    write_1(Ret, FileName).

-spec(write(Node, SupRef, FileName) ->
             ok | {error, Cause} when Node::atom(),
                                      SupRef::supervisor:sup_ref(),
                                      FileName::string(),
                                      Cause::any()).
write(Node, SupRef, FileName) ->
    Ret = info(Node, SupRef),
    write_1(Ret, FileName).

%% @private
write_1({ok, RetL}, FileName) ->
    leo_file:file_unconsult(FileName, RetL);
write_1({error, Cause},_FileName) ->
    {error, Cause}.
