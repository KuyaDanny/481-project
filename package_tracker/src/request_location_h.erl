-module(request_location_h).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, Opts) ->
    {ok, Data, _} = cowboy_req:read_body(Req0),
    % io:format("request location"),
    Package_id = decode_helper(jsx:decode(Data)),
    % io:format("~w~n", [Package_id]),
    Package_Data = request_location:req_loc(Package_id), % shouldn't this look ike 'reply, packageData/or/error, ridkPid?
    % io:format("~w~n", [Package_Data]),
    Response_Data = jsx:encode(encode_helper(Package_Data)),
    % io:format("~w~n", [Response_Data]),
    Req = cowboy_req:reply(
        200,
        #{
            <<"content-type">> => <<"text/json">>
        },
        Response_Data,
        Req0
    ),
    {ok, Req, Opts}.

decode_helper(Data) ->
    try {_, Package_id} = maps:find(<<"package_id">>, Data) of
        _ -> Package_id
    catch
        _:_ -> error
    end.

encode_helper(Package_Data) ->
    Response = case Package_Data of 
        {Lat, Lon, _, History} -> 
            Revised_lat = case is_integer(Lat) or is_float(Lat) of
                true -> Lat;
                _ -> term_to_binary(Lat)
            end,
            Revised_lon = case is_integer(Lon) or is_float(Lon) of
                true -> Lon;
                _ -> term_to_binary(Lon)
            end,

            % io:format("~w~n", [History]),
            % history needs mapped to be json objects, not tuple
            History_convert_fun = fun({Location_id, Time, Status}) ->
                #{<<"location_id">> => Location_id, <<"time">> => Time, <<"status">> => Status}
                end,
            Revised_history = lists:map(History_convert_fun, History),
            #{<<"lat">> => Revised_lat,
                <<"lon">> => Revised_lon,
                <<"history">> => Revised_history};
        error -> 
            #{<<"status">> => <<"error retrieving data">>}
    end,
    Response.


