-module(buffer_api).
-export([enter_center/4,register_package/4, put_on_vehicle/4, mark_delivered/3, request_location/2, vehicle_location_update/4]).

% DO I need to make this a gen_sever with an init that initializes the Riak_PID like in getFriendsServer.
% No, I think that works elsewhere. StoreFriendsServer also initializes it in itself.

% need one to 'get package info'

enter_center(Package_id, Location_id, Time, Riak_PID) ->
    % fetches history from <<packages>>, updates it, then fetches from <<vehicles>> and removes package_id to the list
    Package_Data = case riakc_pb_socket:get(Riak_PID, <<"packages">>, Package_id) of 
	    {ok,Fetched}->
		%reply with the value as a binary, not the key nor the bucket.
		    binary_to_term(riakc_obj:get_value(Fetched));
	    _ ->
		    error
	end,

    Reply = case Package_Data of
        {Lat, Lon, Current_vehicle, History} ->
            Request=riakc_obj:new(<<"packages">>, Package_id, {Lat, Lon, not_on_vehicle, History++[{Location_id, Time, arrived}]}),
            riakc_pb_socket:put(Riak_PID, Request),

            Vehicle_Data = case riakc_pb_socket:get(Riak_PID, <<"vehicles">>, Current_vehicle) of
                {ok, Retrieved} ->
                    binary_to_term(riakc_obj:get_value(Retrieved));
                _ ->
                    error
                end,

            Vehicle_reply = case Vehicle_Data of 
                {Packages} ->
                    Vehicle_request=riakc_obj:new(<<"vehicles">>, Current_vehicle, {Packages--[Package_id]}),
                    riakc_pb_socket:put(Riak_PID, Vehicle_request);
                error ->
                    error
                end;
        error ->
            error
        end,

    % Request=riakc_obj:new(<<"packages">>, Package_id, {null, null, [{Location_id, Time, arrived}]}),
    {reply,Reply,Riak_PID}.

mark_delivered(_Package_id, _Time, _Riak_PID) ->
    % fetches history from <<packages>>, updates it with empty location string, then fetches from <<vehicles>> and removes package_id to the list
    ok.

put_on_vehicle(Package_id, Vehicle_id, Time, Riak_PID) ->
    % fetches history from <<packages>>, updates it, then fetches from <<vehicles>> and adds package_id to the list
    Package_Data = case riakc_pb_socket:get(Riak_PID, <<"packages">>, Package_id) of 
	    {ok,Fetched}->
		%reply with the value as a binary, not the key nor the bucket.
		    binary_to_term(riakc_obj:get_value(Fetched));
	    _ ->
		    error
	end,

    Reply = case Package_Data of
        {Lat, Lon, _, History} ->
            {Location_id, _, _} = lists:last(History),
            Request=riakc_obj:new(<<"packages">>, Package_id, {Lat, Lon, Vehicle_id, History++[{Location_id, Time, departed}]}),
            riakc_pb_socket:put(Riak_PID, Request),
            Vehicle_Data = case riakc_pb_socket:get(Riak_PID, <<"vehicles">>, Vehicle_id) of
                {ok, Retrieved} ->
                    binary_to_term(riakc_obj:get_value(Retrieved));
                _ ->
                    error
                end,

            Vehicle_reply = case Vehicle_Data of 
                {Packages} ->
                    Vehicle_request=riakc_obj:new(<<"vehicles">>, Vehicle_id, {Packages++[Package_id]}),
                    riakc_pb_socket:put(Riak_PID, Vehicle_request);
                error ->
                    io:format("~w~n", [Vehicle_Data]),
                    io:format("~w~n", [Vehicle_id]),
                    io:format("~w~n", [Package_id]),
                    Vehicle_request=riakc_obj:new(<<"vehicles">>, Vehicle_id, {[Package_id]}),
                    riakc_pb_socket:put(Riak_PID, Vehicle_request)
                end;
        error ->
            error
        end,
    
    {reply,Reply,Riak_PID}.

register_package(Package_id, Location_id, Time, Riak_PID) ->
    %<<"packages">>, Package_id, {lat, lon, [{center, time, arrived/departed/deliverd}]})
    % all others fetch the package with a given ID first from the database,
    %   then pull out lat, long, history, etc, update the things that need updated, reuse others, and send it back in
    Request=riakc_obj:new(<<"packages">>, Package_id, {null, null, not_on_vehicle, [{Location_id, Time, arrived}]}),
    {reply,riakc_pb_socket:put(Riak_PID, Request),Riak_PID}.
    % riakc_pb_socket:ping(Riak_PID).

request_location(Package_id, Riak_PID) ->
    % only one where we're returning back actual information with a get()
    Package_Data = case riakc_pb_socket:get(Riak_PID, <<"packages">>, Package_id) of 
	    {ok,Fetched}->
		%reply with the value as a binary, not the key nor the bucket.
		    binary_to_term(riakc_obj:get_value(Fetched));
	    _ ->
		    error
	end,
    {reply,Package_Data,Riak_PID}.

vehicle_location_update(_Vehicle_id, _Lat, _Lon, _Riak_PID) ->

    % case currentVehicle of
    %     not_on_vehicle -> end?
    % vehicles are their own bucket, and have a lat,lon, and list of packages
    % fetch from vehicle thing, then for each package, do a riak call and, get info, update lat/lon, send back with reused.
    ok. % use this to also detach package from vehicle? Maybe track current vehicle in packages riak?

% anything else we might need for those transactions that make calls to riak twice

% 198.199.84.9 needs to go in each gen_server's init.