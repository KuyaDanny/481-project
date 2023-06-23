-module(put_on_vehicle_h).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, Opts) ->

	{ok,Data,_} = cowboy_req:read_body(Req0),
	[Package_id, Vehicle_id, Time|_] = jsx:decode(Data),
	put_on_vehicle:put(Package_id, Vehicle_id, Time),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, "[\"done\"]", Req0),
	{ok, Req, Opts}.