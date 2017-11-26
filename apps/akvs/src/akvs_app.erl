%%%-------------------------------------------------------------------
%% @doc akvs public API
%% @end
%%%-------------------------------------------------------------------

-module(akvs_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    akvs:init(),
    start_http_api(),
    akvs_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

start_http_api() ->
  Dispatch = cowboy_router:compile([{'_',
                                     [{"/kv/:namespace/:key", akvs_h_kv, []}]}]),
  HttpPort = 8080,
  HttpAcceptors = 100,

  {ok, _} = cowboy:start_clear(akvs_http_listener,
    [{port, HttpPort}, {num_acceptors, HttpAcceptors}],
    #{env => #{dispatch => Dispatch}}),
  ok.
