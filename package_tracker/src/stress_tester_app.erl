%%%-------------------------------------------------------------------
%% @doc stress_tester public API
%% @end
%%%-------------------------------------------------------------------

-module(stress_tester_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    stress_tester_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
