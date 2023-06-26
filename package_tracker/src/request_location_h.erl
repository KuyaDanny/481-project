-module(request_location_h).
-behaviour(cowboy_handler).
-export([init/2]).


init(Req0, Opts) ->

	{ok,Data,_} = cowboy_req:read_body(Req0),
	Package_id = helper(jsx:decode(Data)),
	Package_info = jsx:encode(request_location:req_loc(Package_id)),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, Package_info, Req0),
	{ok, Req, Opts}.

helper(Data) ->
    try
        {_,Package_id} = maps:find(<<"Package_id">>, Data)
        of _ -> Package_id
    catch
        _:_ -> error
    end.
