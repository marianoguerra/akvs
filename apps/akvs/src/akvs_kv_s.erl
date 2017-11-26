-module(akvs_kv_s).

%% this module implements the gen_server behaviour
-behaviour(gen_server).

-export([start_link/1, stop/1]).

%% API
-export([set/3, get/2, get/3, del/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

set(Ref, Key, Value) ->
  gen_server:call(Ref, {set, Key, Value}).

get(Ref, Key) ->
  gen_server:call(Ref, {get, Key}).

get(Ref, Key, DefaultValue) ->
  gen_server:call(Ref, {get, Key, DefaultValue}).

del(Ref, Key) ->
  gen_server:call(Ref, {del, Key}).

stop(Ref) ->
  gen_server:call(Ref, stop).

init(Opts) ->
    {ok, KvState} = akvs_kv:new(Opts),
    State = #{kv => KvState} ,
    {ok, State}.

handle_call({set, Key, Value}, _From, State=#{kv := Kv}) ->
    case akvs_kv:set(Kv, Key, Value) of
        {ok, Kv1} -> {reply, ok, State#{kv := Kv1}}
    end;

handle_call({del, Key}, _From, State=#{kv := Kv}) ->
    case akvs_kv:del(Kv, Key) of
        {ok, Kv1} -> {reply, ok, State#{kv := Kv1}}
    end;

handle_call({get, Key}, _From, State=#{kv := Kv}) ->
    Res = akvs_kv:get(Kv, Key),
    {reply, Res, State};

handle_call({get, Key, DefaultValue}, _From, State=#{kv := Kv}) ->
    Res = akvs_kv:get(Kv, Key, DefaultValue),
    {reply, Res, State};

handle_call(stop, _From, State=#{kv := Kv}) ->
    ok = akvs_kv:dispose(Kv),
    {stop, normal, stopped, maps:remove(kv, State)}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State=#{kv := Kv}) ->
    ok = akvs_kv:dispose(Kv);
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
