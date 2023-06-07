-module(buffer_api).
-export([enter_center/4,register_package/4]).


enter_center(_Package_id, _Center_id, _Time, _Riak_PID) ->
    ok.

% mark_delivered

% put_on_vehicle

register_package(_Package_id, _Location, _Time, _Riak_PID) ->
    ok.

% request_location

% vehicle_location_update


% anything else we might need for those transactions that make calls to riak twice