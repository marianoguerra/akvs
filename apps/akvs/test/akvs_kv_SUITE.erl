-module(akvs_kv_SUITE).
-compile(export_all).

%% this function is called by common test and should return a list of all
%% the functions that run tests
all() -> [new_dispose, get_inexistent, get_inexistent_default, get_existent,
          get_updated_existent, get_deleted, set_deleted].

% helper function to do the setup and teardown for us
with_kv(Fn) ->
    {ok, State} = akvs_kv:new(#{}),
    State1 = Fn(State),
    ok = akvs_kv:dispose(State1).

new_dispose(_) ->
    {ok, State} = akvs_kv:new(#{}),
    ok = akvs_kv:dispose(State).

get_inexistent(_) ->
    with_kv(fun (StateIn) ->
                    {error, {not_found, _, _}} = akvs_kv:get(StateIn, <<"k1">>),
                    StateIn
            end).

get_inexistent_default(_) ->
    with_kv(fun (StateIn) ->
                    {ok, default_value} = akvs_kv:get(StateIn, <<"k1">>,
                                                      default_value),
                    StateIn
            end).

get_existent(_) ->
    with_kv(fun (StateIn) ->
                    K = <<"k1">>,
                    {ok, StateOut} = akvs_kv:set(StateIn, K, k1_value),
                    {ok, k1_value} = akvs_kv:get(StateIn, K),
                    StateOut
            end).

get_updated_existent(_) ->
    with_kv(fun (StateIn) ->
                    K = <<"k1">>,
                    {ok, State1} = akvs_kv:set(StateIn, K, k1_value),
                    {ok, k1_value} = akvs_kv:get(StateIn, K),
                    {ok, State2} = akvs_kv:set(State1, K, k1_value_updated),
                    {ok, k1_value_updated} = akvs_kv:get(State2, K),
                    State2
            end).

get_deleted(_) ->
    with_kv(fun (StateIn) ->
                    K = <<"k1">>,
                    {ok, State1} = akvs_kv:set(StateIn, K, k1_value),
                    {ok, k1_value} = akvs_kv:get(State1, K),
                    {ok, State2} = akvs_kv:del(State1, K),
                    {error, {not_found, _, _}} = akvs_kv:get(State2, K),
                    State2
            end).

set_deleted(_) ->
    with_kv(fun (StateIn) ->
                    K = <<"k1">>,
                    {ok, State1} = akvs_kv:set(StateIn, K, k1_value),
                    {ok, k1_value} = akvs_kv:get(State1, K),
                    {ok, State2} = akvs_kv:del(State1, K),
                    {error, {not_found, _, _}} = akvs_kv:get(State2, K),
                    {ok, State3} = akvs_kv:set(State2, K, k1_value_again),
                    {ok, k1_value_again} = akvs_kv:get(State3, K),
                    State3
            end).
