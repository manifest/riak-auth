# Riak Auth

[![Build Status][travis-img]][travis]

Authentication and identity management on top of Riak KV



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
%% Specifying buckets and opening a connection.
Index = <<"riakauth_account_idx">>,
Bucket = {<<"riakauth_account_t">>, <<"riakauth-account">>},
{ok, Pid} = riakc_pb_socket:start_link("192.168.99.100", 8087).

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
