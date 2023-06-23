-module(enter_center_h).

-export([init/2]).


init(Req0, Opts) ->

	{ok,Data,_} = cowboy_req:read_body(Req0),
	[Package_id, Location_id, Time|_] = jsx:decode(Data),
	enter_center:enter(Package_id, Location_id, Time),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, "[\"done\"]", Req0),
	{ok, Req, Opts}.
