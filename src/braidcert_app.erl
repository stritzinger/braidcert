%%%-------------------------------------------------------------------
%% @doc braidcert public API
%% @end
%%%-------------------------------------------------------------------

-module(braidcert_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    braidcert_api:start_handler(),
    braidcert_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
