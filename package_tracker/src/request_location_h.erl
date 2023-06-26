-module(request_location_h).
-behaviour(cowboy_handler).
-export([init/2]).


init(Req0, Opts) ->

	{ok,Data,_} = cowboy_req:read_body(Req0),
	Package_id = decode_helper(jsx:decode(Data)),
	{_, Package_Data, _} = request_location:req_loc(Package_id),
    io:format("~w~n", [Package_Data]),
    Response_Data = jsx:encode(Package_Data),
    io:format("~w~n", [Response_Data]),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, Response_Data, Req0),
	{ok, Req, Opts}.

decode_helper(Data) ->
    try
        {_,Package_id} = maps:find(<<"Package_id">>, Data)
        of _ -> Package_id
    catch
        _:_ -> error
    end.

encode_helper(Package_Data) ->
    {Lat, Lon, History} = Package_Data,
    History.

