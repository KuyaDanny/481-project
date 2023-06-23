-module(mark_delivered_h).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, Opts) ->

	{ok,Data,_} = cowboy_req:read_body(Req0),
	[Package_id, Time|_] = jsx:decode(Data),
	mark_delivered:mark(Package_id,Time),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, "[\"done\"]", Req0),
	{ok, Req, Opts}.