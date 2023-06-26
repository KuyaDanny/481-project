-module(register_package_h).
-behaviour(cowboy_handler).
-export([init/2]).


init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	{Package_id, Location_id, Time} = helper(jsx:decode(Data)), 

    % Package_Data = get_package_server:get_info(Package_id)

    % case is_map(Package_Data) of
    %     true ->
    %         Reply = server_store_package:update(command, {Dat,a, Data})
    %     _ ->
    %         Reply = "err0or"
    %     end,

	register_package:register(Package_id, Location_id, Time),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, "[\"done\"]", Req0),
	{ok, Req, Opts}.


helper(Data) ->
    try
        {_,Package_id} = maps:find(<<"Package_id">>, Data),
        {_,Location_id} = maps:find(<<"Location_id">>, Data),
        {_,Time} = maps:find(<<"Time">>, Data) 
        of _ -> {Package_id, Location_id, Time}
    catch
        _:_ -> {error, error, error}
    end.