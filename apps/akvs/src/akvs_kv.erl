-module(akvs_kv).
-export([new/1, dispose/1, set/3, get/2, get/3, del/2]).
-export_type([error/0, key/0, value/0, state/0]).

% the error type for all our functions is the same,
% it's a tuple with the error % atom as first item and a three item tuple as
% second item,
% where the first is an atom identifying the type of error for other code,
% the second is a human readable string-like value describing the error and the
% third is more context about the error
-type error() :: {error, {atom(), iolist(), map()}}.
-type key()   :: binary().
-type value() :: any().

% we don't want other modules to know/care about the internal structure of
% the state type
-opaque state() :: map().

%% @doc create a new instance of a key value store
-spec new(map()) -> {ok, state()} | error().
new(_Opts) ->
    Table = ets:new(akvs_kv, [set, {write_concurrency, false},
                                   {read_concurrency, false}]),
    State = #{table => Table},
    {ok, State}.

%% @doc dispose resources associated with a previously created kv store
-spec dispose(state()) -> ok | error().
dispose(#{table := Table}) ->
    true = ets:delete(Table),
    ok.

%% @doc set a value for a key in a kv store
-spec set(state(), key(), value()) -> {ok, state()} | error().
set(State=#{table := Table}, Key, Value) ->
    true = ets:insert(Table, {Key, Value}),
    {ok, State}.

%% @doc get a value for a key or an error if not found
-spec get(state(), key()) -> {ok, value()} | error().
get(#{table := Table}, Key) ->
    case ets:lookup(Table, Key) of
        [] -> {error, {not_found, "Key not found", #{key => Key}}};
        [{Key, Value}] -> {ok, Value}
    end.

%% @doc get a value for a key or a default value if not found
-spec get(state(), key(), value()) -> {ok, value()} | error().
get(#{table := Table}, Key, DefaultValue) ->
    case ets:lookup(Table, Key) of
        [] -> {ok, DefaultValue};
        [{Key, Value}] -> {ok, Value}
    end.

%% @doc remove a value for a key, if not found do nothing
-spec del(state(), key()) -> {ok, state()} | error().
del(StateIn=#{table := Table}, Key) ->
    true = ets:delete(Table, Key),
    {ok, StateIn}.
