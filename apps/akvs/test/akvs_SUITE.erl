-module(akvs_SUITE).
-compile(export_all).

%% this function is called by common test and should return a list of all
%% the functions that run tests
all() -> [get_inexistent, get_inexistent_default, get_existent,
          get_updated_existent, get_deleted, set_deleted].

init_per_testcase(_, Config) ->
    % we need to setup the ETS table per test case since it seems it dissapears
    % after each test case
    akvs:init(),
    Config.

end_per_suite(_, _Config) ->
    ok.

get_inexistent(_) ->
    Ns = <<"ns1">>,
    {error, {not_found, _, _}} = akvs:get(Ns, <<"k1">>).

get_inexistent_default(_) ->
    Ns = <<"ns2">>,
    {ok, default_value} = akvs:get(Ns, <<"k1">>, default_value).

get_existent(_) ->
    Ns = <<"ns3">>,
    K = <<"k1">>,
    ok = akvs:set(Ns, K, k1_value),
    {ok, k1_value} = akvs:get(Ns, K).

get_updated_existent(_) ->
    Ns = <<"ns4">>,
    K = <<"k1">>,
    ok = akvs:set(Ns, K, k1_value),
    {ok, k1_value} = akvs:get(Ns, K),
    ok = akvs:set(Ns, K, k1_value_updated),
    {ok, k1_value_updated} = akvs:get(Ns, K).

get_deleted(_) ->
    Ns = <<"ns5">>,
    K = <<"k1">>,
    ok = akvs:set(Ns, K, k1_value),
    {ok, k1_value} = akvs:get(Ns, K),
    ok = akvs:del(Ns, K),
    {error, {not_found, _, _}} = akvs:get(Ns, K).

set_deleted(_) ->
    Ns = <<"ns6">>,
    K = <<"k1">>,
    ok = akvs:set(Ns, K, k1_value),
    {ok, k1_value} = akvs:get(Ns, K),
    ok = akvs:del(Ns, K),
    {error, {not_found, _, _}} = akvs:get(Ns, K),
    ok = akvs:set(Ns, K, k1_value_again),
    {ok, k1_value_again} = akvs:get(Ns, K).
