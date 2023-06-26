-module(register_package_h).
-behaviour(cowboy_handler).
-export([init/2]).


init(Req0, Opts) ->

	{ok,Data,_} = cowboy_req:read_body(Req0),
	{Package_id, Location_id, Time} = helper(jsx:decode(Data)), % binary to term?
	% #{<<"Package_id">> => Package_id_bin, <<"Location_id">> => Location_id_bin, <<"Time">> => Time} = jsx:decode(Data),
    % it's a record, not a dict.
	% #{<<"Package_id>">> => Package_id_value, Location_id => Location_id_value, Time => 900} = to_list(jsx:decode(Data)),
    % [Package_id, Location_id, Time|_] = jsx:decode(Data), % binary to term?
	% register_package:register(binary_to_term(Package_id_bin), binary_to_term(Location_id_bin), Time),

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