-module(akvs).
-export([init/0]).
-export([set/3, get/2, get/3, del/2]).

-type ns() :: binary().
-type key() :: akvs_kv:key().
-type value() :: akvs_kv:value().
-type error() :: akvs_kv:value().

%% @doc set Key to Value in namespace Ns
-spec set(ns(), key(), value()) -> ok | error().
set(Ns, Key, Value) ->
    {ok, Ref} = get_kv_s(Ns),
    akvs_kv_s:set(Ref, Key, Value).

%% @doc get Key from namespace Ns
-spec get(ns(), key()) -> {ok, value()} | error().
get(Ns, Key) ->
    {ok, Ref} = get_kv_s(Ns),
    akvs_kv_s:get(Ref, Key).

%% @doc get Key from namespace Ns or DefaultValue if Key not found
-spec get(ns(), key(), value()) -> {ok, value()} | error().
get(Ns, Key, DefaultValue) ->
    {ok, Ref} = get_kv_s(Ns),
    akvs_kv_s:get(Ref, Key, DefaultValue).

%% @doc delete  Key in namespace Ns
-spec del(ns(), key()) -> ok | error().
del(Ns, Key) ->
    {ok, Ref} = get_kv_s(Ns),
    akvs_kv_s:del(Ref, Key).

-spec init() -> ok.
init() ->
    ets:new(akvsreg, [set, public, named_table,
                          {write_concurrency, true},
                          {read_concurrency, true}]),
    ok.

%% Private functions
get_kv_s(Ns) ->
    case ets:lookup(akvsreg, Ns) of
        [{Ns, Ref}] -> {ok, Ref};
        [] -> register_kv_s(Ns)
    end.

register_kv_s(Ns) ->
    {ok, Ref} = akvs_kv_s:start(#{}),
    true = ets:insert(akvsreg, {Ns, Ref}),
    {ok, Ref}.
