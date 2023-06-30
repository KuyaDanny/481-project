-module(put_on_vehicle_h).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, Opts) ->

	{ok,Data,_} = cowboy_req:read_body(Req0),
    io:format("put on vehicle"),
	{Package_id, Vehicle_id, Time} = helper(jsx:decode(Data)),
	put_on_vehicle:put(Package_id, Vehicle_id, Time),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, "[\"done\"]", Req0),
	{ok, Req, Opts}.

helper(Data) ->
    try
        {_,Package_id} = maps:find(<<"package_id">>, Data),
        {_,Vehicle_id} = maps:find(<<"vehicle_id">>, Data),
        {_,Time} = maps:find(<<"timestamp">>, Data) 
        of _ -> {Package_id, Vehicle_id, Time}
    catch
        _:_ -> {error, error, error}
    end.
