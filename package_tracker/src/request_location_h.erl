-module(request_location_h).
-behaviour(cowboy_handler).
-export([init/2]).


init(Req0, Opts) ->

	{ok,Data,_} = cowboy_req:read_body(Req0),
	[Package_id|_] = jsx:decode(Data),
	request_location:req_loc(Package_id),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, "[\"done\"]", Req0),
	{ok, Req, Opts}.
