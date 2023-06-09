-module(buffer_api).
-export([enter_center/4,register_package/4, put_on_vehicle/4, mark_delivered/3, request_location/2, vehicle_location_update/4]).


enter_center(_Package_id, _Center_id, _Time, _Riak_PID) ->
    ok.

mark_delivered(_Package_id, _Time, _Riak_PID) ->
    ok.

put_on_vehicle(_Package_id, _Vehicle_id, _Time, _Riak_PID) ->
    ok.

register_package(_Package_id, _Location, _Time, _Riak_PID) ->
    ok.

request_location(_Package_id, _Riak_PID) ->
    ok.

vehicle_location_update(_Vehicle_id, _Lat, _Lon, _Riak_PID) ->
    ok.


% anything else we might need for those transactions that make calls to riak twice