# Riak Auth

[![Build Status][travis-img]][travis]

Authentication and identity management on top of Riak KV


### Why you might consider to use this library

- Reliability.
	By default, strict quorum (pr=quorum) of vnodes is required for
	create or update operations on accounts. You would always know if operation is failed.
- Availability.
	By default, sloppy quorum of vnodes is used for read operations on accounts.
	You would able to authenticate user's requests even when all but one vnode fail.
- Horizontal scalability.
	Being based on Riak KV, the number of account entries aren't important anymore.
- High performance.
	Just one request 'by key' after Solr query are used for authenticate operations.
- Flexibility.
	Create, link and unlink identities to/from accounts.
	The identities conflict resolution scheme is provided out of the box.
	It's on your own to choose where to store accounts data and how to query them,
	by specifying bucket and index names of Riak KV.
- Extensibility.
	Custom data can be embedded within account entries.
- Support for Solr-based Riak search 2.0.
	If you need to perform queries through accounts data, schemas are available.


### Overview

Riak Auth is an extendable authentication framework that stored accounts data in Riak KV.
The idea is to link authentication identities (globally unique identifiers) to accounts.
It makes it possible to find account by identity in the future.

The identity could be any list of binary strings:
- `[<<"oauth2">>, <<"google-plus">>, <<"1">>]` or `[<<"oauth1">>, <<"twitter">>, <<"2">>]`
	could be used to describe: a protocol, a provider identifier and a user's identifier
	in case of Social Login;
- `[<<"password">>, <<"John:xyz">>]` could represent a username with a hash of password;
- `[<<"uniqlink">>, <<"https://example.org?ref=123">>]` for an unique shareable link.

With the library you can not only authenticate users but also link identities
of different external services. In other words, social accounts of Google Plus
and Facebook could be linked within one account along with an ability
to use a username and a password for the login.

There are no restrictions on the format of identities. The best practice,
to use from one to many first segments for something we call **authentication key**
(part of the identity that describe an identity provider itself and has nothing in common
with a particular user) and the very last segment for a **user identifier** or **user's credentials**.
That rules will make indentities more compact for storing and simple for querying.

### How To Use

To build and start playing with the library, execute following shell commands:

```bash
## Building the development image and running the container with Riak KV within it.
$ ./run-docker.sh
## Building the application and executing an erlang shell.
$ make app shell
```

The library provides just one function for account lookup and creation.
It expects a verified user's identity to lookup user's account and
a function that generates unique id for new accounts.
A new or an existed user's account will be returned.

```erlang
riakauth:authenticate(
  Pid, Bucket, Index,                            %% account bucket and search index
  [<<"oauth2">>, <<"google-plus">>, <<"123">>],  %% user's identity
  fun() -> <<"unique id">> end).                 %% should generate unique id
```

In the following example, you will see how you can use the library
to manage multiple user's identities.

```erlang
%% Initializing a connection to Riak KV.
{ok, S, _} = erl_scan:string(os:getenv("DEVELOP_ENVIRONMENT")),
{ok, Conf} = erl_parse:parse_term(S),
#{kv_protobuf := #{host := Host, port := Port}} = Conf,
{ok, Pid} = riakc_pb_socket:start_link(Host, Port).

%% Specifying index and bucket
Index = <<"riakauth_account_idx">>,
Bucket = {<<"riakauth_account_t">>, <<"riakauth-account">>}.

%% The "riakauth:authenticate/{5,6,7}" function above creates accounts,
%% but we can also use functions from "riakauth_account" module for that purpose.
%% Let's add to John's account a few identities.
GoogleIdentity = [<<"oauth2">>, <<"google-plus">>, <<"1">>],
FacebookIdentity = [<<"oauth2">>, <<"facebook">>, <<"5">>],
A0 = riakauth_account:new_dt(),
A1 = riakauth_account:update_identity_dt(GoogleIdentity, A0),
A2 = riakauth_account:update_identity_dt(FacebookIdentity, A1),
riakauth_account:put(Pid, Bucket, <<"John">>, A2).

%% The "riakauth:authenticate/{5,6,7}" function returns John's account for both identities for now.
riakauth:authenticate(Pid, Bucket, Index, GoogleIdentity, fun() -> <<"42">> end).
%% {<<"John">>,...
riakauth:authenticate(Pid, Bucket, Index, FacebookIdentity, fun() -> <<"42">> end).
%% {<<"John">>,...

%% John can decide to unlink identity from one account and link it to another.
{_Key, B0} = riakauth:authenticate(Pid, Bucket, Index, FacebookIdentity, fun() -> <<"42">> end),
B1 = riakauth_account:remove_identity_dt(FacebookIdentity, B0),
riakauth_account:put(Pid, Bucket, <<"John">>, B1).
riakauth:authenticate(Pid, Bucket, Index, FacebookIdentity, fun() -> <<"NewJohn">> end).
%% {<<"NewJohn">>,...

%% If there is the same identity in different accounts,
%% the "riakauth:authenticate/{5,6,7}" function will return
%% an account with the oldest identity.
C0 = riakauth_account:get(Pid, Bucket, <<"NewJohn">>),
C1 = riakauth_account:update_identity_dt(GoogleIdentity, C0),
riakauth_account:put(Pid, Bucket, <<"NewJohn">>, C1).
riakauth:authenticate(Pid, Bucket, Index, GoogleIdentity, fun() -> <<"42">> end).
%% {<<"John">>,...

%% The user should remove his identity from the old account to use it with a new one.
riakauth_account:put(
  Pid, Bucket, <<"John">>,
  riakauth_account:remove_identity_dt(
    GoogleIdentity,
    riakauth_account:get(Pid, Bucket, <<"John">>))).
riakauth:authenticate(Pid, Bucket, Index, GoogleIdentity, fun() -> <<"42">> end).
%% {<<"NewJohn">>,...
```

### License

The source code is provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[travis]:https://travis-ci.org/manifest/riak-auth?branch=master
[travis-img]:https://secure.travis-ci.org/manifest/riak-auth.png
