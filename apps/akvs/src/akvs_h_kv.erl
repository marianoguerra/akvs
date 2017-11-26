-module(akvs_h_kv).
-export([init/2]).

init(ReqIn=#{method := <<"POST">>}, State) ->
    {Ns, Key} = get_url_vars(ReqIn),
    {ok, Body, Req1} = cowboy_req:read_body(ReqIn),
    ok = akvs:set(Ns, Key, Body),
    ReqOut = cowboy_req:reply(201, #{}, <<"Created">>, Req1),
    {ok, ReqOut, State};

init(ReqIn=#{method := <<"GET">>}, State) ->
    {Ns, Key} = get_url_vars(ReqIn),
    ReqOut = case akvs:get(Ns, Key) of
                 {ok, Value} ->
                     cowboy_req:reply(200, #{}, Value, ReqIn);
                 {error, {not_found, _, _}} ->
                     cowboy_req:reply(404, #{}, <<"Not Found">>, ReqIn)
             end,
    {ok, ReqOut, State};

init(ReqIn=#{method := <<"DELETE">>}, State) ->
    {Ns, Key} = get_url_vars(ReqIn),
    ok = akvs:del(Ns, Key),
    ReqOut = cowboy_req:reply(200, #{}, <<"OK">>, ReqIn),
    {ok, ReqOut, State};

init(ReqIn, State) ->
    ReqOut = cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, ReqIn),
    {ok, ReqOut, State}.

%% Private functions

get_url_vars(Req) ->
  Ns = cowboy_req:binding(namespace, Req),
  Key = cowboy_req:binding(key, Req),
  {Ns, Key}.
