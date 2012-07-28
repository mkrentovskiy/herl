-module(gpio).
-behaivour(gen_server).

-export([start/1, dir/1, read/0, write/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {pin = 0, dir = in, vf = undefine}).

% Interface

start(P) -> gen_server:start_link({local, ?MODULE}, ?MODULE, P, []).
dir(D) -> gen_server:call(?MODULE, {dir, D}).
read() -> gen_server:call(?MODULE, read).
write(Val) -> gen_server:call(?MODULE, {write, Val}).

% Realisation

init({Pin, D}) -> 
	{ok, ValF} = gpio_seize(Pin, D),
	{ok, #state{pin = Pin, dir = D, vf = ValF}}.

handle_call({dir, D}, _From, S) when D =/= S#state.dir ->
	gpio_dir(S#state.pin, D),
	{reply, ok, S};

handle_call(read, _From, S) ->
	Val = gpio_read(S#state.vf),
	{reply, Val, S};

handle_call({write, Val}, _From, S) ->
	gpio_write(S#state.vf, Val),
	{reply, ik, S};

handle_call(_Msg, _From, S) -> {reply, ok, S}.
handle_cast(_Msg, S) -> {noreply, S}.
handle_info(_Msg, S) -> {noreply, S}.

terminate(_Reason, {Pin, _, ValF}) -> 	
	gpio_close(ValF), 
	gpio_release(Pin), 
	ok.
code_change(_OldVsn, S, _Extra) -> {ok, S}.

% GPIO 

gpio_seize(Pin, D) ->
	gpio_release(Pin),
	{ok, ExpF} = file:open("/sys/class/gpio/export", [write]),
	file:write(ExpF, integer_to_list(Pin)),
	file:close(ExpF),
	gpio_dir(Pin, D),
	gpio_open(Pin).

gpio_open(Pin) ->
	file:open("/sys/class/gpio/gpio" ++ integer_to_list(Pin) ++ "/value", [read, write]).

gpio_close(ValF) -> 
	file:close(ValF).

gpio_dir(Pin, D) ->
	{ok, DirF} = file:open("/sys/class/gpio/gpio" ++ integer_to_list(Pin) ++ "/direction", [write]),
	case D of
		in  -> file:write(DirF, "in");
		out -> file:write(DirF, "out")
  	end,
  	file:close(DirF).

gpio_write(ValF, Val) ->
	file:position(ValF, 0),
	file:write(ValF, integer_to_list(Val)).

gpio_read(ValF) ->
	file:position(ValF, 0),
	{ok, Val} = file:read(ValF, 1),
	Val.

gpio_release(Pin) ->
	{ok, UexpF} = file:open("/sys/class/gpio/unexport", [write]),
	file:write(UexpF, integer_to_list(Pin)),
	file:close(UexpF).
