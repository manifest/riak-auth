%% ----------------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2016-2017 Andrei Nesterov <ae.nesterov@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to
%% deal in the Software without restriction, including without limitation the
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%% sell copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%% ----------------------------------------------------------------------------

-module(riakauth).

-include_lib("riakc/include/riakc.hrl").

%% API
-export([
	authenticate/5,
	authenticate/6,
	authenticate/7,
	unix_time_us/0,
	unix_time_us/1
]).

%% =============================================================================
%% API
%% =============================================================================

-spec authenticate(pid(), bucket_and_type(), binary(), riakauth_account:identity(), fun(() -> binary())) -> {binary(), riakauth_account:account()}.
authenticate(Pid, Bucket, Index, Identity, HandleKey) ->
	authenticate(Pid, Bucket, Index, Identity, HandleKey, fun(Obj) -> Obj end).

-spec authenticate(pid(), bucket_and_type(), binary(), riakauth_account:identity(), fun(() -> binary()), fun((riakauth_account:data()) -> riakauth_account:data())) -> {binary(), riakauth_account:account()}.
authenticate(Pid, Bucket, Index, Identity, HandleKey, HandleData) ->
	authenticate(Pid, Bucket, Index, Identity, HandleKey, HandleData, unix_time_us()).

-spec authenticate(pid(), bucket_and_type(), binary(), riakauth_account:identity(), fun(() -> binary()), fun((riakauth_account:data()) -> riakauth_account:data()), non_neg_integer()) -> {binary(), riakauth_account:account()}.
authenticate(Pid, Bucket, Index, Identity, HandleKey, HandleData, Time) ->
	case riakauth_account:find_identity(Pid, Index, Identity) of
		{ok, Key} ->
			case riakauth_account:find(Pid, Bucket, Key) of
				{ok, A} -> {Key, A};
				_       -> exit({obsolete_index, Index})
			end;
		_ ->
			case riakauth_account:find(Pid, Bucket, Key = HandleKey(), [{pr, quorum}]) of
				{ok, _} -> exit({obsolete_index, Index});
				_ ->
					A0 = riakauth_account:new_dt(),
					A1 = riakauth_account:update_identity_dt(Identity, Time, A0),
					A2 = riakauth_account:update_data_dt(HandleData, A1),
					_ = riakauth_account:put(Pid, Bucket, Key, A2),
					{Key, A2}
			end
	end.

-spec unix_time_us() -> non_neg_integer().
unix_time_us() ->
	unix_time_us(erlang:timestamp()).

-spec unix_time_us(erlang:timestamp()) -> non_neg_integer().
unix_time_us({MS, S, US}) ->
	MS * 1000000000000 + S * 1000000 + US.
