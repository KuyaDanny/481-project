-module(vehicle_location_update_h).
-behaviour(cowboy_handler).
-export([init/2]).


init(Req0, Opts) ->

	{ok,Data,_} = cowboy_req:read_body(Req0),
	{Vehicle_id, Latitude, Longitude} = helper(jsx:decode(Data)),
	vehicle_location_update:update(Vehicle_id, Latitude, Longitude),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, "[\"done\"]", Req0),
	{ok, Req, Opts}.

helper(Data) ->
    try
        {_,Vehicle_id} = maps:find(<<"Vehicle_id">>, Data),
        {_,Lat} = maps:find(<<"Lat">>, Data),
        {_,Lon} = maps:find(<<"Lon">>, Data) 
        of _ -> {Vehicle_id, Lat, Lon}
    catch
        _:_ -> {error, error, error}
    end.