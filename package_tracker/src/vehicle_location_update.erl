%% @author Lee Barney
%% @copyright 2022 Lee Barney licensed under the <a>
%%        rel="license"
%%        href="http://creativecommons.org/licenses/by/4.0/"
%%        target="_blank">
%%
%% These solutions are not intended to be ideal solutions. Instead,
%% they are a solution that you can compare against yours to see
%% other options and to come up with even better solutions.
-module(vehicle_location_update).
-behaviour(gen_server).

%% API
-export([start_link/0,stop/0,update/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @spec start -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
stop() -> gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%% @spec start -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
update(Vehicle_id, Latitude, Longitude)-> gen_server:cast(?MODULE, {vehicle_location_update, {Vehicle_id, Latitude, Longitude}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	riakc_pb_socket:start_link("rdb.fordark.org", 8087). % TODO change
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages. The Request parameter is a tuple
%% consisting of a command, a binary that is the name of the person 
%% for which friends are to be stored, and a binary that is a list 
%% of friends. Both of these binaries can be created using term_to_binary.
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(stop, _From, _State) ->
	{stop,normal,
                server_stopped,
          down}. %% setting the server's internal state to down

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({vehicle_location_update, {Vehicle_id, Lat, Lon}}, Riak_PID) when is_list(Vehicle_id) 
                                                                        andalso is_number(Lat) 
                                                                        andalso is_number(Lon) ->
    {_Result} = buffer_api:vehicle_location_update(Vehicle_id, Lat, Lon, Riak_PID),
    {noreply, Riak_PID};

handle_cast({vehicle_location_update, _Args}, State) ->
    {reply, bad_arg, State};

handle_cast(_Msg, State) ->
    {reply, bad_message, State}.

% takes in vehicle_id, lat, and lon. Lat and lon are reals, and Vehicle_id is string.
% handle: 'when' Lat is numb andalso Lon isNum andalso  isString(Vehicle_id)

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-ifdef(EUNIT).
  -include_lib("eunit/include/eunit.hrl").

handle_cast_test_() ->
    {setup,
        fun()-> 
			meck:new(buffer_api),
			meck:expect(buffer_api, vehicle_location_update, fun(Vehicle_id, Lat, Lon, Riak_PID) -> {worked} end)
		end,
		fun(_)-> 
			meck:unload(buffer_api)
		end,
    [%This is the list of tests to be generated and run.
        ?_assertEqual({noreply, some_riak_pid},
                            vehicle_location_update:handle_cast({vehicle_location_update,{"vehicle2800", 11, 10.94}}, some_riak_pid)),
        ?_assertEqual({noreply,some_riak_pid},
                            vehicle_location_update:handle_cast({vehicle_location_update,{"302jdja947", 420, 420}}, some_riak_pid)),
        ?_assertEqual({reply,bad_arg,some_riak_pid},
                            vehicle_location_update:handle_cast({vehicle_location_update,{"MagicSchoolBus", mojave_desert}}, some_riak_pid)),
        ?_assertEqual({reply,bad_message,some_riak_pid},
                            vehicle_location_update:handle_cast({game_over}, some_riak_pid))
    ]}.
%  
%component_level_test_()->{
%  setup,
%  fun()->gen_server:start_link({local, ?SERVER}, ?MODULE, [], []) end,
%  fun()->gen_server:call(?SERVER, stop) end,
%  [?_assertEqual(true,true)]}.

  -endif.