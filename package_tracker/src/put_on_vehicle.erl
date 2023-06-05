%% @author Lee Barney
%% @copyright 2022 Lee Barney licensed under the <a>
%%        rel="license"
%%        href="http://creativecommons.org/licenses/by/4.0/"
%%        target="_blank">
%%
%% These solutions are not intended to be ideal solutions. Instead,
%% they are a solution that you can compare against yours to see
%% other options and to come up with even better solutions.
-module(put_on_vehicle).
-behaviour(gen_server).

%% API
-export([start_link/0,stop/0,set_friends_for/2]).

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
set_friends_for(Name,Friends)-> gen_server:call(?MODULE, {friends_for,Name,Friends}).

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
handle_call({friends_for,B_name,B_friends}, _From, Riak_Pid) ->
	Request=riakc_obj:new(<<"friends">>, B_name, B_friends),
	{reply,riakc_pb_socket:put(Riak_Pid, Request),Riak_Pid};
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
handle_cast(_Msg, State) ->
    {noreply, State}.

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

handle_call_test_()->
  [?_assertEqual({reply,
                {ok,[joe,sally,grace]},
           [joe,sally,grace]},friends_storage:handle_call(list,somewhere,[joe,sally,grace]))%happy path
   ].

	% handle_cast_test_()->
	% 	[?_assertEqual({noreply,[sue,joe,sally]},friends_storage:handle_cast({add,sue},[joe,sally])),%happy path
	%    ?_assertEqual({noreply,[sue]},friends_storage:handle_cast({add,sue},[])),%nasty path
	%    ?_assertEqual({noreply,[sue]},friends_storage:handle_cast({add,sue},nil)),%nasty path
	% 	 ?_assertEqual({noreply,
	%                 ok,
	%            [joe,grace]},friends_storage:handle_cast({remove,sally},[joe,sally,grace]))%happy path

	% 	].
%component_level_test_()->{
%  setup,
%  fun()->gen_server:start_link({local, ?SERVER}, ?MODULE, [], []) end,
%  fun()->gen_server:call(?SERVER, stop) end,
%  [?_assertEqual(true,true)]}.

  -endif.