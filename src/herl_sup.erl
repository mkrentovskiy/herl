
-module(herl_sup).
-behaviour(supervisor).

-include("herl.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, P), {I, {I, start_link, [P]}, permanent, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Pins = lists:map(fun({Pin, D}) -> 
			Name = list_to_atom("gpio" ++ integer_to_list(Pin)),
			{Name, {gpio, start, [{Pin, D}]}, permanent, 5000, worker, [gpio]} 
		end, ?GPIOs), 
    {ok, { {one_for_one, 5, 10}, Pins} }.

