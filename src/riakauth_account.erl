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
%% furnished to do so, entry to the following conditions:
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

-module(riakauth_account).

-include_lib("riakc/include/riakc.hrl").

%% API
-export([
	find_identity/3,
	find/3,
	find/4,
	get/3,
	get/4,
	get/5,
	put/4,
	put/5,
	remove/3,
	remove/4
]).

%% DataType API
-export([
	new_dt/0,
	update_dt/3,
	update_dt/4,
	data_rawdt/1,
	update_data_dt/2,
	identity_rawdt/2,
	find_identity_rawdt/2,
	update_identity_dt/2,
	update_identity_dt/3,
	remove_identity_dt/2,
	fold_identities_dt/4
]).

%% Definitions
-define(DEFAULT_REQUEST_TIMEOUT, 5000).

%% Types
-type data()     :: riakc_map:crdt_map().
-type account()  :: riakc_map:crdt_map().
-type identity() :: [binary()].
-type rawdt()    :: {{binary(), riakc_datatype:datatype()}, any()}.

-export_type([account/0, data/0, identity/0, rawdt/0]).

%% =============================================================================
%% API
%% =============================================================================

-spec find_identity(pid(), binary(), identity()) -> {ok, binary()} | error.
find_identity(Pid, Index, Identity) ->
	IdentityPath = identity_path(Identity),
	Query = <<"*:*">>,
	Qopts =
		[	{filter, <<IdentityPath/binary, "cat_register:*">>},
			{sort, <<IdentityPath/binary, "cat_register asc">>},
			{rows, 1} ],
	case catch riakc_pb_socket:search(Pid, Index, Query, Qopts, ?DEFAULT_REQUEST_TIMEOUT) of
			{ok, {_, [{_, Doc}], _, _}} ->
				{_, Id} = lists:keyfind(<<"_yz_rk">>, 1, Doc),
				{ok, Id};
			{ok, _}          -> error;
			{error, Reason}  -> exit(Reason);
			{'EXIT', Reason} -> exit(Reason);
			Else             -> exit({bad_return_value, Else})
	end.

-spec get(pid(), bucket_and_type(), binary()) -> account().
get(Pid, Bucket, Key) ->
	get(Pid, Bucket, Key, []).

-spec get(pid(), bucket_and_type(), binary(), [proplists:property()]) -> account().
get(Pid, Bucket, Key, Opts) ->
	case find(Pid, Bucket, Key, Opts) of
		{ok, Val} -> Val;
		_         -> error({bad_key, Bucket, Key})
	end.

-spec get(pid(), bucket_and_type(), binary(), [proplists:property()], account()) -> account().
get(Pid, Bucket, Key, Opts, Default) ->
	case find(Pid, Bucket, Key, Opts) of
		{ok, Val} -> Val;
		_         -> Default
	end.

-spec find(pid(), bucket_and_type(), binary()) -> {ok, account()} | error.
find(Pid, Bucket, Key) ->
	find(Pid, Bucket, Key, []).

-spec find(pid(), bucket_and_type(), binary(), [proplists:property()]) -> {ok, account()} | error.
find(Pid, Bucket, Key, Opts) ->
	case catch riakc_pb_socket:fetch_type(Pid, Bucket, Key, Opts) of
		{ok, Val}                  -> {ok, Val};
		{error, {notfound, _Type}} -> error;
		{error, Reason}            -> exit(Reason);
		{'EXIT', Reason}           -> exit(Reason);
		Else                       -> exit({bad_return_value, Else})
	end.

-spec put(pid(), bucket_and_type(), binary(), account()) -> account().
put(Pid, Bucket, Key, A) ->
	put(Pid, Bucket, Key, A, []).

-spec put(pid(), bucket_and_type(), binary(), account(), [proplists:property()]) -> account().
put(Pid, Bucket, Key, A, Opts) ->
	case catch riakc_pb_socket:update_type(Pid, Bucket, Key, riakc_map:to_op(A), [{pw, quorum}|Opts]) of
		ok                  -> A;
		{ok, Amodified}     -> Amodified;
		{error, unmodified} -> A;
		{error, Reason}     -> exit(Reason);
		{'EXIT', Reason}    -> exit(Reason);
		Else                -> exit({bad_return_value, Else})
	end.

-spec remove(pid(), bucket_and_type(), binary()) -> ok.
remove(Pid, Bucket, Id) ->
	remove(Pid, Bucket, Id, []).

-spec remove(pid(), bucket_and_type(), binary(), [proplists:property()]) -> ok.
remove(Pid, Bucket, Id, Opts) ->
	case catch riakc_pb_socket:delete(Pid, Bucket, Id, Opts) of
		ok               -> ok;
		{error, Reason}  -> exit(Reason);
		{'EXIT', Reason} -> exit(Reason);
		Else             -> exit({bad_return_value, Else})
	end.

%% =============================================================================
%% DataType API
%% =============================================================================

-spec new_dt() -> account().
new_dt() ->
	riakc_map:new().

-spec update_dt(identity(), fun((data()) -> data()), account()) -> account().
update_dt(Identity, HandleData, A) ->
	update_dt(Identity, HandleData, riakauth:unix_time_us(), A).

-spec update_dt(identity(), fun((data()) -> data()), non_neg_integer(), account()) -> account().
update_dt(Identity, HandleData, CreatedAt, A0) ->
	A1 = update_identity_dt(Identity, CreatedAt, A0),
	update_data_dt(HandleData, A1).

-spec identity_rawdt(identity(), account()) -> [rawdt()].
identity_rawdt(Identity, A) ->
	case find_identity_rawdt(Identity, A) of
		{ok, Val} -> Val;
		_         -> error({bad_key, Identity})
	end.

-spec find_identity_rawdt(identity(), account()) -> {ok, [rawdt()]} | error.
find_identity_rawdt(Identity, A) ->
	case riakc_map:find({<<"auth">>, map}, A) of
		{ok, Raw} -> find_in_rawdt(Identity, Raw);
		_         -> error
	end.

-spec data_rawdt(account()) -> any().
data_rawdt(A) ->
	riakc_map:fetch({<<"data">>, map}, A).

-spec update_data_dt(fun((data()) -> data()), account()) -> account().
update_data_dt(HandleData, A) ->
	riakc_map:update({<<"data">>, map}, HandleData, A).

-spec update_identity_dt(identity(), account()) -> account().
update_identity_dt(Identity, A) ->
	update_identity_dt(Identity, riakauth:unix_time_us(), A).

-spec update_identity_dt(identity(), non_neg_integer(), account()) -> account().
update_identity_dt(Identity, CreatedAt, A) ->
	update_in_dt(
		[<<"auth">>|Identity],
		fun(I) ->
			riakc_map:update({<<"cat">>, register}, fun(Obj) -> riakc_register:set(integer_to_binary(CreatedAt), Obj) end, I)
		end,
		A).

-spec remove_identity_dt(identity(), account()) -> account().
remove_identity_dt(Identity, A) ->
	case riakc_map:find({<<"auth">>, map}, A) of
		{ok, Raw} ->
			SizesReverted = size_in_rawdt(Identity, Raw, []),
			case length(SizesReverted) =:= length(Identity) of
				true ->
					%% Removing the specified identity and all empty subtrees.
					SegmentNum =
						lists:foldl(
							fun
								(1, 0 =Acc)  -> Acc;
								(_Size, Acc) -> Acc +1
							end,
							0, SizesReverted),
					remove_in_dt([<<"auth">>|lists:sublist(Identity, SegmentNum)], A);
				_ ->
					%% The specified identity isn't fully presented in the account object,
					%% so that our work is done.
					A
			end;
		_ ->
			%% There is no "auth" property in the account object,
			%% so that our work is done.
			A
	end.

-spec fold_identities_dt(fun((identity(), [rawdt()], any()) -> any()), [binary()], any(), account()) -> any().
fold_identities_dt(HandleIdentity, Keys, AccIn, A) ->
	case riakc_map:find({<<"auth">>, map}, A) of
		{ok, Input} ->
			fold_identities_in_dt(HandleIdentity, Keys, AccIn, Input);
		_ ->
			%% There is no "auth" property in the account object,
			%% so that our work is done.
			AccIn
	end.

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec identity_path(identity()) -> binary().
identity_path(Identity) ->
	identity_path(Identity, <<"auth_map.">>).

-spec identity_path(identity(), binary()) -> binary().
identity_path([Val|T], Acc) -> identity_path(T, <<Acc/binary, Val/binary, "_map.">>);
identity_path([], Acc)      -> Acc.

-spec find_in_rawdt(identity(), [rawdt()]) -> {ok, [rawdt()]} | error.
find_in_rawdt([Key|T], Raw0) ->
	case lists:keyfind({Key, map}, 1, Raw0) of
		{_, Raw1} -> find_in_rawdt(T, Raw1);
		_         -> error
	end;
find_in_rawdt([], Raw) ->
	{ok, Raw}.

-spec update_in_dt([binary()], fun((M) -> M), M) -> any() when M :: riakc_map:crdt_map().
update_in_dt([Key|T], Handle, M)  -> riakc_map:update({Key, map}, fun(Obj) -> update_in_dt(T, Handle, Obj) end, M);
update_in_dt([], Handle, M)       -> Handle(M).

-spec size_in_rawdt([binary()], list(), [non_neg_integer()]) -> [non_neg_integer()].
size_in_rawdt([Key|T], Raw0, Acc) ->
	case lists:keyfind({Key, map}, 1, Raw0) of
		{_, Raw1} -> size_in_rawdt(T, Raw1, [length(Raw0)|Acc]);
		_         -> Acc
	end;
size_in_rawdt([], _Raw, Acc) ->
	Acc.

-spec remove_in_dt([binary()], M) -> {non_neg_integer(), M} when M :: riakc_map:crdt_map().
remove_in_dt([], M)      -> M;
remove_in_dt([Key], M)   -> riakc_map:erase({Key, map}, M);
remove_in_dt([Key|T], M) -> riakc_map:update({Key, map}, fun(Obj) -> remove_in_dt(T, Obj) end, M).

-spec fold_identities_in_dt(fun((identity(), [rawdt()], any()) -> any()), [binary()], any(), [rawdt()]) -> any().
fold_identities_in_dt(Handle, Keys, AccIn, Input) ->
	fold_identities_in_dt(Handle, Keys, AccIn, Input, []).

-spec fold_identities_in_dt(fun((identity(), [rawdt()], any()) -> any()), [binary()], any(), [rawdt()], [binary()]) -> any().
fold_identities_in_dt(Handle, Keys0, AccIn0, Input0, Acc) ->
	lists:foldl(
		fun
			({{Segment, map}, Input1}, AccIn1) ->
				case filter_identity_keys(Keys0, Segment) of
					{ok, Keys1} ->
						fold_identities_in_dt(Handle, Keys1, AccIn1, Input1, [Segment|Acc]);
					commit ->
						Handle(lists:reverse([Segment|Acc]), Input1, AccIn1);
					_ ->
						%% The segment's name doesn't match any of the specified keys.
						AccIn1
				end;
			(_, AccIn1) ->
				%% An element of the "auth" map isn't a map,
				%% so that it cannot be a segment.
				AccIn1
		end, AccIn0, Input0).

-spec filter_identity_keys(Keys, binary()) -> {ok, Keys} | commit | error when Keys :: [[binary()]].
filter_identity_keys(Keys, Segment) ->
	filter_identity_keys(Keys, Segment, error, []).

-spec filter_identity_keys(Keys, binary(), ok | commit | error, Keys) -> {ok, Keys} | commit | error when Keys :: [[binary()]].
filter_identity_keys([[Kh|Kt]|T], Segment, _St, Acc) when Kh =:= Segment -> filter_identity_keys(T, Segment, ok, [Kt|Acc]);
filter_identity_keys([[]|T], Segment, _St, Acc)                          -> filter_identity_keys(T, Segment, commit, Acc);
filter_identity_keys([_|T], Segment, St, Acc)                            -> filter_identity_keys(T, Segment, St, Acc);
filter_identity_keys([], _Segment, ok, Acc)                              -> {ok, Acc};
filter_identity_keys([], _Segment, St, _Acc)                             -> St.
