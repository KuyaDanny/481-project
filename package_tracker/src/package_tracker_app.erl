%%%-------------------------------------------------------------------
%% @doc package_tracker public API
%% @end
%%%-------------------------------------------------------------------

-module(package_tracker_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
     Dispatch = cowboy_router:compile([
        { <<"_">>, [{<<"/">>, hello_handler, []}]}
    ]),
     {ok, _} = cowboy:start_clear(
        hello_handler,
        [{port, 8087}],
        #{env => #{dispatch => Dispatch}}
     ),
    package_tracker_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
