-module(akvs_kv_s_SUITE).
-compile(export_all).

%% this function is called by common test and should return a list of all
%% the functions that run tests
all() -> [new_dispose, get_inexistent, get_inexistent_default, get_existent,
          get_updated_existent, get_deleted, set_deleted].

% helper function to do the setup and teardown for us
with_kv(Fn) ->
    {ok, Ref} = akvs_kv_s:start_link(#{}),
    Fn(Ref),
    stopped = akvs_kv_s:stop(Ref).

new_dispose(_) ->
    {ok, Ref} = akvs_kv_s:start_link(#{}),
    stopped = akvs_kv_s:stop(Ref).

get_inexistent(_) ->
    with_kv(fun (Ref) ->
                    {error, {not_found, _, _}} = akvs_kv_s:get(Ref, <<"k1">>)
            end).

get_inexistent_default(_) ->
    with_kv(fun (Ref) ->
                    {ok, default_value} = akvs_kv_s:get(Ref, <<"k1">>, default_value)
            end).

get_existent(_) ->
    with_kv(fun (Ref) ->
                    K = <<"k1">>,
                    ok = akvs_kv_s:set(Ref, K, k1_value),
                    {ok, k1_value} = akvs_kv_s:get(Ref, K)
            end).

get_updated_existent(_) ->
    with_kv(fun (Ref) ->
                    K = <<"k1">>,
                    ok = akvs_kv_s:set(Ref, K, k1_value),
                    {ok, k1_value} = akvs_kv_s:get(Ref, K),
                    ok = akvs_kv_s:set(Ref, K, k1_value_updated),
                    {ok, k1_value_updated} = akvs_kv_s:get(Ref, K)
            end).

get_deleted(_) ->
    with_kv(fun (Ref) ->
                    K = <<"k1">>,
                    ok = akvs_kv_s:set(Ref, K, k1_value),
                    {ok, k1_value} = akvs_kv_s:get(Ref, K),
                    ok = akvs_kv_s:del(Ref, K),
                    {error, {not_found, _, _}} = akvs_kv_s:get(Ref, K)
            end).

set_deleted(_) ->
    with_kv(fun (Ref) ->
                    K = <<"k1">>,
                    ok = akvs_kv_s:set(Ref, K, k1_value),
                    {ok, k1_value} = akvs_kv_s:get(Ref, K),
                    ok = akvs_kv_s:del(Ref, K),
                    {error, {not_found, _, _}} = akvs_kv_s:get(Ref, K),
                    ok = akvs_kv_s:set(Ref, K, k1_value_again),
                    {ok, k1_value_again} = akvs_kv_s:get(Ref, K)
            end).
