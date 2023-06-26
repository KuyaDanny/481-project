-module(request_location_h).
-behaviour(cowboy_handler).
-export([init/2]).


init(Req0, Opts) ->

	{ok,Data,_} = cowboy_req:read_body(Req0),
	Package_id = helper(jsx:decode(Data)),
	Package_data = request_location:req_loc(Package_id),
    io:format(Package_data),
    Response_Data = jsx:encode(term_to_binary(Package_data)),
    io:format(Response_Data),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, Response_Data, Req0),
	{ok, Req, Opts}.

helper(Data) ->
    try
        {_,Package_id} = maps:find(<<"Package_id">>, Data)
        of _ -> Package_id
    catch
        _:_ -> error
    end.
