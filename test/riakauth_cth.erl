-module(riakauth_cth).

-include_lib("riakc/include/riakc.hrl").

%% API
-export([
	init_config/0,
	riakc_open/1,
	has_identity/4,
	make_key/0,
	make_identity/0
]).

%% =============================================================================
%% API
%% =============================================================================

-spec init_config() -> list().
init_config() ->
	Config =
		try
			{ok, S, _} = erl_scan:string(os:getenv("DEVELOP_ENVIRONMENT")),
			{ok, Conf} = erl_parse:parse_term(S),
			maps:fold(fun(Key, Val, Acc) -> [{Key, Val}|Acc] end, [], Conf)
		catch _:Reason -> error({missing_develop_environment, ?FUNCTION_NAME, Reason}) end,
	Index = <<"riakauth_account_idx">>,
	Bucket = {<<"riakauth_account_t">>, <<"riakauth-account">>},
	[{index, Index}, {bucket, Bucket} | Config].

-spec riakc_open(list()) -> pid().
riakc_open(Config) ->
	{_, #{host := Host, port := Port}} = lists:keyfind(kv_protobuf, 1, Config),
	{ok, Pid} = riakc_pb_socket:start_link(Host, Port),
	Pid.

-spec has_identity(pid(), bucket_and_type(), binary(), riakauth_account:identity()) -> boolean().
has_identity(Pid, Bucket, Key, Identity) ->
	case riakauth_account:find_identity_rawdt(Identity, riakauth_account:get(Pid, Bucket, Key)) of
		{ok, _} -> true;
		_       -> false
	end.

-spec make_identity() -> riakauth_account:identity().
make_identity() ->
	[list_to_binary(vector(8, alphanum_chars())) || _ <- lists:seq(1, rand:uniform(5))].

-spec make_key() -> binary().
make_key() ->
	list_to_binary(vector(128, alphanum_chars())).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec oneof(list()) -> integer().
oneof(L) ->
	lists:nth(rand:uniform(length(L)), L).

-spec vector(non_neg_integer(), list()) -> list().
vector(MaxSize, L) ->
	vector(0, MaxSize, L, []).

-spec vector(non_neg_integer(), non_neg_integer(), list(), list()) -> list().
vector(Size, MaxSize, L, Acc) when Size < MaxSize ->
	vector(Size +1, MaxSize, L, [oneof(L)|Acc]);
vector(_, _, _, Acc) ->
	Acc.

-spec alphanum_chars() -> list().
alphanum_chars() ->
	"0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".
