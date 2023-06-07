%% @author Lee Barney
%% @copyright 2022 Lee Barney licensed under the <a>
%%        rel="license"
%%        href="http://creativecommons.org/licenses/by/4.0/"
%%        target="_blank">
%%
%% These solutions are not intended to be ideal solutions. Instead,
%% they are a solution that you can compare against yours to see
%% other options and to come up with even better solutions.
-module(register_package).
-behaviour(gen_server).

%% API
-export([start_link/0,stop/0,set_friends_for/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2,
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
set_friends_for(Name,Friends)-> gen_server:call(?MODULE, {register,Name,Friends}).

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
	riakc_pb_socket:start_link("rdb.fordark.org", 8087).
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
handle_call({register,{Package_id, Location, Time}}, _From, Riak_PID) ->
    % //this will call buffer_api:some_function;
    {Result} = buffer_api:register_package(Package_id, Location, Time, Riak_PID),
    {reply, Result, Riak_PID}; % return all the things we want. {reply, registered, Riak_Pid}

handle_call({register, _Arg}, _From, Riak_PID) -> 
    {reply,bad_arg,Riak_PID};

handle_call({_Cmd, _Arg}, _From, Riak_PID) -> 
    {reply,bad_command,Riak_PID};

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
% handle_cast(_Msg, State) ->
%     {noreply, State}.

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

handle_call_test_() ->
    {setup,
        fun()-> 
			meck:new(buffer_api),
			meck:expect(buffer_api, register_package, fun(Package_id, Location, Time, Riak_PID) -> {registered} end)
		end,
		fun(_)-> 
			meck:unload(buffer_api)
		end,
    [%This is the list of tests to be generated and run.
        ?_assertEqual({reply,registered,some_riak_pid},
                            register_package:handle_call({register,{"package88A","center 13", 400}}, some_from_pid, some_riak_pid)),
        ?_assertEqual({reply,registered,some_riak_pid},
                            register_package:handle_call({register,{"package54B","center 12",""}}, some_from_pid, some_riak_pid)),
        ?_assertEqual({reply,bad_arg,some_riak_pid},
                            register_package:handle_call({register,{"",[]}}, some_from_pid, some_riak_pid)),
        ?_assertEqual({reply,bad_command,some_riak_pid},
                            register_package:handle_call({sticks,{"",[]}}, some_from_pid, some_riak_pid))
    ]}.
%  
%component_level_test_()->{
%  setup,
%  fun()->gen_server:start_link({local, ?SERVER}, ?MODULE, [], []) end,
%  fun()->gen_server:call(?SERVER, stop) end,
%  [?_assertEqual(true,true)]}.


    
-endif.
