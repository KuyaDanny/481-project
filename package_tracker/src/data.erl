-module(data).
-export([build_mark_delivereds/2,build_register_packages/2,build_package_location_requests/2,
	build_put_on_vehicle_requests/2,build_enter_center_requests/2,build_vehicle_location_updates/2,
	generate_UUID/2]).

%%
%% Here are the data transfer definitions used in this version of the app
%%

%Extension: /request_location %% Done
	%to server: {"package_id":UUID}
%Extension: /register_package %% Done
	%to server: {package_id: <UUID>, center_id: <UUID>, timestamp: <Long>}
	%out from server: {"history":[{"holder_uuid":"string","time_stamp":"UINT_32"}]}
%Extension: /put_on_vehicle %% Done
	%to server: {package_id: <UUID>, vehicle_id: <UUID>, timestamp: <Long>}
%Extension: /enter_center %% Done
	%to server: {package_id: <UUID>, center_id: <UUID>, timestamp: <Long>}
	%out from server: {"history":[{"location":{"lat":"real","long":"real"},"time_stamp":"UINT_32"}]}
%Extension: /vehicle_location_update %% 
	%to server: {vehicle_id: <UUID>, lat: <Real>,lon: <Real>}
%Extension: /mark_delivered %% Done
	%to server: {package_id: <UUID>,timestamp: <Long> }


time_in_millis() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).

 

generate_UUID(Domain,Tester_name)->
	uuid:to_string(uuid:uuid5(dns,Domain++atom_to_list(Tester_name)++integer_to_list(erlang:monotonic_time()))).




build_mark_delivereds(UUIDs,Count) ->
	lists:flatten([[#{package_id => list_to_binary(UUID),timestamp => integer_to_list(time_in_millis())} || UUID <- UUIDs]
		|| _ <- lists:seq(1,Count)]).

build_register_packages(Package_UUIDs,Count)->
	lists:flatten([[#{package_id => list_to_binary(UUID),center_id=>list_to_binary(generate_UUID("stuff.com",silly)),
			timestamp => integer_to_list(time_in_millis())} || UUID <- Package_UUIDs]
		|| _ <- lists:seq(1,Count)]).

build_package_location_requests(Package_UUIDs,Count)->
	lists:flatten([[#{package_id => UUID}
								|| UUID <- Package_UUIDs] || _ <- lists:seq(1,Count)]).

build_put_on_vehicle_requests(Package_UUIDs,Count)->
	lists:flatten([[#{package_id=>list_to_binary(UUID),vehicle_id=>list_to_binary(generate_UUID("vehicle.com",silly)),
	timestamp=>integer_to_list(time_in_millis())} || UUID<- Package_UUIDs] 
		|| _ <- lists:seq(1,Count)]).

build_enter_center_requests(Package_UUIDs,Count)->
	lists:flatten([[#{package_id=>list_to_binary(UUID),center_id=>list_to_binary(generate_UUID("center.com",silly)),
	    timestamp=>integer_to_list(time_in_millis())} || UUID <- Package_UUIDs] 
	    || _ <- lists:seq(1,Count)]).


build_vehicle_location_updates(Vehicle_UUIDs,Count)->
	lists:flatten([[#{vehicle_id=>list_to_binary(UUID),
			lat=>integer_to_list(round((rand:uniform_real()*360))),
			lon=>integer_to_list(round(rand:uniform_real()*360))}
		|| UUID <- Vehicle_UUIDs] || _ <- lists:seq(1,Count)]).







	

