%%%-------------------------------------------------------------------
%% @doc package_tracker public API
%% @end
%%%-------------------------------------------------------------------

-module(package_tracker_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
     Dispatch = cowboy_router:compile([
        {'_', [
            {"/", hello_handler, []},
            {"/enter_center",enter_center_h,[]},
            {"/mark_delivered",mark_delivered_h,[]},
            {"/put_on_vehicle",put_on_vehicle_h,[]},
            {"/register_package",register_package_h,[]}, % might work we get a 500 error so maybe stuff with the riak database
            {"/request_location",request_location_h,[]},
            {"/vehicle_location_update",vehicle_location_update_h,[]}
        ]}
    ]),

    PrivDir = code:priv_dir(package_tracker),
    {ok,_} = cowboy:start_tls(https_listener, [
   		{port, 443},
		{certfile, PrivDir ++ "/ssl/fullchain.pem"}, % is this used to ssh to the riak database?
		{keyfile, PrivDir ++ "/ssl/privkey.pem"}
	], #{env => #{dispatch => Dispatch}}),
    package_tracker_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
