-module(enter_center_h).
-behaviour(cowboy_handler).
-export([init/2]).


init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
    io:format("enter center"),
	{Package_id, Location_id, Time} = helper(jsx:decode(Data)), 

    % Package_Data = get_package_server:get_info(Package_id),

    % case is_map(Package_Data) of
    %     true ->
    %         Reply = server_store_package:update(command, {Data, Data, Data}, RIAK_PID)
    %     _ ->
    %         Reply = "err0or"
    %     end,

	enter_center:enter(Package_id, Location_id, Time),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, "[\"done\"]", Req0),
	{ok, Req, Opts}.

helper(Data) ->
    try
        {_,Package_id} = maps:find(<<"package_id">>, Data),
        {_,Location_id} = maps:find(<<"center_id">>, Data),
        {_,Time} = maps:find(<<"timestamp">>, Data) 
        of _ -> {Package_id, Location_id, Time}
    catch
        _:_ -> {error, error, error}
    end.