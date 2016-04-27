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
%% Records
-record(state, {
          callback = entop_format :: atom(),
          remote_module = entop_collector :: atom(),
          columns :: [],
          cbstate,
          node :: atom(),
          otp_version :: atom(),
          erts_version :: atom(),
          os_fam,
          os = [] :: string(),
          os_version = [] :: string(),
          node_flags = [] :: [{atom(), any()}],
          reverse_sort = true :: boolean(),
          sort = 1 :: non_neg_integer(),
          topn = 10 :: non_neg_integer(),
          interval = 1 :: pos_integer(),
          times = 1 :: pos_integer(),
          root_sup :: atom(),
          expected_svt = [] :: [term()],
          last_reductions = [] :: list()
         }).
%% Defines
-define(PRINT(Str), io:format(user, Str, [])).
-define(PRINTF(FStr, Args), io:format(user, FStr, Args)).
