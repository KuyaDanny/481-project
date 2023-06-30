-module(stresser).
-export([start_all/2]).
-compile(export_all).

start_tracker()->
	Initial_time = os:system_time(millisecond),
	Tracker_pid = spawn(stresser,track_times,[{Initial_time,Initial_time,0}]),
	register(tracker,Tracker_pid).

transactions_per_second()->
	tracker ! {self(),get},
	receive
		{Initial_time,Last_time,Count}=Result -> io:format("~p transactions/second~n",[Count* 1000 / (Last_time - Initial_time)]);
		Oops->io:format("Error: got ~p~n",[Oops])
	end.

track_times({Initial_time,Stored_latest_time,Count}=State)->
	Next_state = receive
		update -> {Initial_time,os:system_time(millisecond),Count+1};
		{Pid,get} -> Pid ! State,%Count/(Stored_latest_time - Initial_time),
					 State %reuse the same state
	end,
	track_times(Next_state).



%%
%% Each package will go through 10 facility-to-vehicle changes.
%% Each of these 10 vehicles will go through 1,000 location changes.
%%
start_all(Package_count,Domain)->
	Tester_name = cool_guy,
	Http_info = {post %request type
	 ,string:concat("https://",Domain)%URL
     ,[]%Header
     ,"application/json"%Type
     ,[{ssl, [{customize_hostname_check, 
                [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}]}]%HttpOptions
     ,[]},%Options

	build_and_spawn_all(Domain,Tester_name,Package_count,Http_info).

build_and_spawn_all(Domain,Tester_name,Package_count,Http_info)->
	%build all the requests
	Package_uuids = [data:generate_UUID(Domain,Tester_name) || _ <- lists:seq(1,Package_count)],
	Vehicle_uuids = [data:generate_UUID(Domain,Tester_name) || _ <- lists:seq(1,Package_count)],
	Package_registration_requests = data:build_register_packages(Package_uuids,1000),
	Put_on_vehicle_requests = data:build_put_on_vehicle_requests(Package_uuids,1000),
	Enter_center_requests = data:build_enter_center_requests(Package_uuids,1000),
	Vehicle_location_updates = data:build_vehicle_location_updates(Vehicle_uuids,100_000),
	Package_location_requests = data:build_package_location_requests(Package_uuids,1000),
	Delivered_requests = data:build_mark_delivereds(Package_uuids,1000),
	
	%start the tracker
	start_tracker(),

	%spawn all the requests
	spawn_register_packages(Package_registration_requests,Http_info),
	spawn_put_on_vehicle_requests(Put_on_vehicle_requests,Http_info),
	spawn_enter_center_requests(Enter_center_requests, Http_info),
	spawn_vehicle_location_updates(Vehicle_location_updates, Http_info),
	spawn_package_location_requests(Package_location_requests,Http_info),
	spawn_delivered_requests(Delivered_requests,Http_info).


spawn_register_packages(Package_registration_requests, Http_info)->
	[spawn(fun()-> send(Q,Http_info,"/register_package") end) || Q <- Package_registration_requests].

spawn_put_on_vehicle_requests(Put_on_vehicle_requests, Http_info)->
	[spawn(fun()-> send(Q,Http_info,"/put_on_vehicle") end) || Q <- Put_on_vehicle_requests].

spawn_enter_center_requests(Enter_center_requests, Http_info)->
	[spawn(fun()-> send(Q,Http_info,"/enter_center") end) || Q <- Enter_center_requests].

spawn_vehicle_location_updates(Vehicle_location_updates,Http_info)->
	[spawn(fun()-> send(Q,Http_info,"/vehicle_location_update") end) || Q <- Vehicle_location_updates].

spawn_package_location_requests(Package_location_requests,Http_info)->
	[spawn(fun()-> send(Q,Http_info,"/request_location") end) || Q <- Package_location_requests].

spawn_delivered_requests(Delivered_requests,Http_info)->
	[spawn(fun()-> send(Q,Http_info,"/mark_delivered") end) || Q <- Delivered_requests].





send(Body,{Method,URL,Header,Type,HTTPOptions,Options},URL_extension)->
	%make the request and ignore the response
    httpc:request(Method, {string:concat(URL,URL_extension), Header, Type, jsx:encode(Body)}, HTTPOptions, Options),
    %update the tracker when a response is received
    tracker ! update.



