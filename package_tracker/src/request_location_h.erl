-module(request_location_h).
-behaviour(cowboy_handler).
-export([init/2]).


init(Req0, Opts) ->

	{ok,Data,_} = cowboy_req:read_body(Req0),
	Package_id = decode_helper(jsx:decode(Data)),
	Package_Data = request_location:req_loc(Package_id),
    io:format("~w~n", [Package_Data]),
    Response_Data = jsx:encode(encode_helper(Package_Data)),
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

    Revised_lat = case is_integer(Lat) or is_float(Lat) of
        true -> Lat;
        _ -> <<Lat>>
    end,
    Revised_lon = case is_integer(Lon) or is_float(Lon) of
        true -> Lon;
        _ -> <<Lon>>
    end,

    % history needs mapped to be json objects, not tuple
    History_convert_fun = fun({Location_id, Time, Status}) ->
        #{<<"location_id">> => <<Location_id>>, <<"time">> => Time, <<"Status">> => <<Status>>}
        end,
    Revised_history = lists:map(History, History_convert_fun),
    Response = #{<<"lat">> => Revised_lat,
                <<"lon">> => Revised_lon,
                "history" => Revised_history},

    Response.


