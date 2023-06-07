-module(riak_api).
-export([get_package/2, get_vehicle/2, get_eta/2]).
-export([put_vehicle_location/5]).


get_vehicle(_Vehicle_id, Riak_PID) ->
    ok.

get_package(_Package_id, Riak_PID) ->
    ok.

get_eta(_Package_id, Riak_PID) ->
    ok.

put_vehicle_location(_Vehicle_id, _Lat, _Lon, _Time, Riak_PID) ->
    ok.