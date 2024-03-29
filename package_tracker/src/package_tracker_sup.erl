%%%-------------------------------------------------------------------
%% @doc package_tracker top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(package_tracker_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [child(register_package,worker),
                    child(put_on_vehicle, worker),
                    child(enter_center, worker),
                    child(mark_delivered, worker),
                    child(vehicle_location_update, worker),
                    child(request_location, worker)],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
child(Module,Type)->
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
	#{id => Module,
	  start => {Module,start_link,[]},
	  restart => permanent,
	  shutdown => 2000,
	  type => Type,
	  modules => [Module]}.
