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

-module(main_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%% =============================================================================
%% Common Test callbacks
%% =============================================================================

all() ->
	application:ensure_all_started(riakauth),
	[{group, main}].

groups() ->
	[{main, [parallel], ct_helper:all(?MODULE)}].

init_per_suite(Config) ->
	riakauth_cth:init_config() ++ Config.

end_per_suite(Config) ->
	Config.

%% =============================================================================
%% Tests
%% =============================================================================

%% Identity can be added and removed.
identity_roundtrip(Config) ->
	Bucket = ?config(bucket, Config),
	Key = riakauth_cth:make_key(),
	Identity = riakauth_cth:make_identity(),

	Pid = riakauth_cth:riakc_open(Config),
	riakauth_account:put(
		Pid, Bucket, Key,
		riakauth_account:update_identity_dt(
			Identity,
			riakauth_account:new_dt())),
	true = riakauth_cth:has_identity(Pid, Bucket, Key, Identity),

	riakauth_account:put(
		Pid, Bucket, Key,
		riakauth_account:remove_identity_dt(
			Identity,
			riakauth_account:get(Pid, Bucket, Key))),
	false = riakauth_cth:has_identity(Pid, Bucket, Key, Identity).

%% Identities of the account can be queried by authentication keys.
query_identities(Config) ->
	Bucket = ?config(bucket, Config),
	Key = riakauth_cth:make_key(),
	IdentityAProt = riakauth_cth:make_identity_segment(),
	IdentityAProtProv1 = riakauth_cth:make_identity_segment(),
	IdentityAProtProv2 = riakauth_cth:make_identity_segment(),
	IdentityAProtUid1 = riakauth_cth:make_identity_segment(),
	IdentityAProtUid2 = riakauth_cth:make_identity_segment(),
	IdentityAProtUid3 = riakauth_cth:make_identity_segment(),
	IdentityAKey1 = [IdentityAProt, IdentityAProtProv1],
	IdentityAKey2 = [IdentityAProt, IdentityAProtProv2],
	IdentityA1 = [IdentityAProt, IdentityAProtProv1, IdentityAProtUid1],
	IdentityA2 = [IdentityAProt, IdentityAProtProv1, IdentityAProtUid2],
	IdentityA3 = [IdentityAProt, IdentityAProtProv2, IdentityAProtUid3],
	IdentityBProt = riakauth_cth:make_identity_segment(),
	IdentityBProtCredentials1 = riakauth_cth:make_identity_segment(),
	IdentityBProtCredentials2 = riakauth_cth:make_identity_segment(),
	IdentityBKey = [IdentityAProt],
	IdentityB1 = [IdentityBProt, IdentityBProtCredentials1],
	IdentityB2 = [IdentityBProt, IdentityBProtCredentials2],
	Handle = fun(Segment, Acc) -> [Segment|Acc] end,
	A0 = riakauth_account:new_dt(),
	A1 = riakauth_account:update_identity_dt(IdentityA1, A0),
	A2 = riakauth_account:update_identity_dt(IdentityA2, A1),
	A3 = riakauth_account:update_identity_dt(IdentityB1, A2),
	A4 = riakauth_account:update_identity_dt(IdentityB2, A3),
	Test =
		[	%% No authkeys - no identities
			{[], []},
			%% All protocols (top-level segments of authkeys)
			{[[]], [[IdentityAProt], [IdentityBProt]]},
			%% All identities
			{[IdentityAKey1, IdentityAKey2, IdentityBKey], [IdentityA1, IdentityA2, IdentityA3, IdentityB1, IdentityB2]},
			%% Filter by the specified authkey
			{[IdentityAKey1], [IdentityA1, IdentityA2]} ],

	Pid = riakauth_cth:riakc_open(Config),
	A = riakauth_account:put(Pid, Bucket, Key, A4, [return_body]),
	[begin
		[] =
			lists:subtract(
				riakauth_account:fold_identities_dt(Handle, Keys, [], A),
				Identities)
	end || {Keys, Identities} <- Test].

%% Account will be created, if the specified identity hasn't presented yet.
no_identity(Config) ->
	Index = ?config(index, Config),
	Bucket = ?config(bucket, Config),
	Key = riakauth_cth:make_key(),
	HandleKey = fun() -> Key end,
	Identity = riakauth_cth:make_identity(),

	Pid = riakauth_cth:riakc_open(Config),
	{Key, _A} = riakauth:authenticate(Pid, Bucket, Index, Identity, HandleKey).

%% Account will be returned, if the specified identity is presented.
one_identity(Config) ->
	Index = ?config(index, Config),
	Bucket = ?config(bucket, Config),
	Key = riakauth_cth:make_key(),
	HandleKey = fun() -> <<"42">> end,
	Identity = riakauth_cth:make_identity(),

	Pid = riakauth_cth:riakc_open(Config),
	riakauth_account:put(
		Pid, Bucket, Key,
		riakauth_account:update_identity_dt(
			Identity,
			riakauth_account:new_dt())),

	do_wait(),
	{Key, A} = do_retry(fun() -> riakauth:authenticate(Pid, Bucket, Index, Identity, HandleKey) end),
	{ok, _} = riakauth_account:find_identity_rawdt(Identity, A).

%% Account can be found by any identity that has been linked to it.
same_identities_same_accounts(Config) ->
	Index = ?config(index, Config),
	Bucket = ?config(bucket, Config),
	Key = riakauth_cth:make_key(),
	HandleKey = fun() -> <<"42">> end,
	HandleData = fun(Data) -> Data end,
	Aident = riakauth_cth:make_identity(),
	Bident = riakauth_cth:make_identity(),
	Test = [Aident, Bident],

	Pid = riakauth_cth:riakc_open(Config),
	riakauth_account:put(
		Pid, Bucket, Key,
		riakauth_account:update_identity_dt(
			Aident,
			riakauth_account:new_dt())),
	riakauth_account:put(
		Pid, Bucket, Key,
		riakauth_account:update_identity_dt(
			Bident,
			riakauth_account:new_dt())),

	do_wait(),
	[begin
		{Key, A} = do_retry(fun() -> riakauth:authenticate(Pid, Bucket, Index, Identity, HandleKey, HandleData) end),
		{ok, _} = riakauth_account:find_identity_rawdt(Identity, A)
	end || Identity <- Test].

%% The oldest account will be returned, if the specified identity has been linked to different accounts.
same_identities_different_accounts(Config) ->
	Index = ?config(index, Config),
	Bucket = ?config(bucket, Config),
	Akey = riakauth_cth:make_key(),
	Bkey = riakauth_cth:make_key(),
	HandleKey = fun() -> <<"42">> end,
	HandleData = fun(Data) -> Data end,
	Aident = riakauth_cth:make_identity(),
	Bident = riakauth_cth:make_identity(),
	Time = 1,

	Pid = riakauth_cth:riakc_open(Config),
	riakauth_account:put(
		Pid, Bucket, Akey,
		riakauth_account:update_identity_dt(
			Aident,
			Time,
			riakauth_account:new_dt())),
	riakauth_account:put(
		Pid, Bucket, Bkey,
		riakauth_account:update_identity_dt(
			Bident,
			Time +1,
			riakauth_account:new_dt())),

	do_wait(),
	{Akey, A} = do_retry(fun() -> riakauth:authenticate(Pid, Bucket, Index, Aident, HandleKey, HandleData, Time +2) end),
	{ok, _} = riakauth_account:find_identity_rawdt(Aident, A).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec do_wait() -> ok.
do_wait() ->
	timer:sleep(3000).

-spec do_retry(function()) -> ok.
do_retry(Authenticate) ->
	try Authenticate()
	catch
		exit:{obsolete_index, _} ->
			timer:sleep(100),
			do_retry(Authenticate)
	end.
