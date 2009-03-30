-module(agents).
-export([main/0, main/1, agent/3, barrier_control/3, location/1, view/3]).

-import(lists, [keytake/3, nth/2, reverse/1, unzip/1]).

-define(N_CYCLES, 1024).
-define(WORLD_SIZE, 5).
-define(LOC_AGENTS, 12).
-define(LOC_SIZE, 4096).
-define(LOC_AREA, ?LOC_SIZE * ?LOC_SIZE).
-define(LOC_MAX, (?LOC_SIZE div 2) - 1).
-define(LOC_MIN, -(?LOC_SIZE div 2)).
-define(OFFSETS,
  [{-1, -1}, {0, -1}, {1, -1}, {-1, 0}, {1, 0}, {-1, 1}, {0, 1}, {1, 1}, {0, 0}]
).

%% Vectors

vector_add({X1, Y1}, {X2, Y2}) -> {X1 + X2, Y1 + Y2}.
vector_sub({X1, Y1}, {X2, Y2}) -> {X1 - X2, Y1 - Y2}.


%% Barrier

release_waiting([]) ->
  nop;
release_waiting([P|PS]) ->
  P ! sync,
  release_waiting(PS).

barrier_control(Enrolled, Count, Waiting) ->
  receive
    {sync, P} -> 
      if
        Count > 1 ->
	  barrier_control(Enrolled, Count - 1, [P|Waiting]);
	true      ->
	  release_waiting([P|Waiting]),
	  barrier_control(Enrolled, Enrolled, [])
      end;
    {enroll, N} ->
      barrier_control(Enrolled + N, Count + N, Waiting);
    {resign, N} ->
      if
        (Count - N) == 0 ->
	  release_waiting(Waiting),
	  barrier_control(Enrolled - N, Enrolled - N, []);
	true ->
	  barrier_control(Enrolled - N, Count - N, Waiting)
      end;
    shutdown ->
      nop
  end.

init_barrier(Count) -> 
  register(barrier, spawn(agents, barrier_control, [Count, Count, []])).

sync_barrier() ->
  barrier ! {sync, self()},
  receive
    sync ->
      nop
  end.

shutdown_barrier() ->
  barrier ! shutdown.


%% Locations and Views

make_nv({X, Y}) -> {X * ?LOC_SIZE, Y * ?LOC_SIZE}.
get_neighbour(N, Neighbourhood) -> element(N + 1, Neighbourhood).

dir_value(N) ->
  if
    N < ?LOC_MIN -> -1;
    N > ?LOC_MAX -> 1;
    true -> 0
  end.

find_offset(V, N, []) ->
  {N, V};
find_offset(V, N, [O|OS]) ->
  if
    V == O -> {N, O};
    true   -> find_offset(V, (N + 1), OS)
  end.

calculate_direction({X, Y}) ->
  find_offset({dir_value(X), dir_value(Y)}, 0, ?OFFSETS).

in_bounds(Vector) ->
  {X, Y} = Vector,
  if
    X < ?LOC_MIN; X > ?LOC_MAX; Y < ?LOC_MIN; Y > ?LOC_MAX -> false;
    true -> true
  end.

adjust_agents(_, [], Result) ->
  Result;
adjust_agents(V, [A|AS], Result) ->
  {ID, Loc, Pos} = A,
  NewPos = vector_add(Pos, V),
  adjust_agents(V, AS, [{ID, Loc, NewPos}|Result]).

build_view([], [], Agents) ->
  Agents;
build_view([O|OS], [L|LS], Agents) -> 
  L ! {get_info, self()},
  receive
    {info, AgentList} ->
      NewAgents = adjust_agents(make_nv(O), AgentList, Agents),
      build_view(OS, LS, NewAgents)
  end.

view(Search, Cycle, Agents) ->
  receive
    {get_view, R, ReqCycle} ->
      if
        ReqCycle == Cycle ->
	  R ! Agents,
	  view(Search, Cycle, Agents);
	true ->
	  NewAgents = build_view(?OFFSETS, Search, []),
	  R ! NewAgents,
	  view(Search, ReqCycle, NewAgents)
      end;
    shutdown ->
      nop
  end.

location_loop(Loc, Neighbours, View, Agents) ->
  receive
    {enter, R, Info} ->
      {ID, _, Pos} = Info,
      Local = in_bounds(Pos),
      if
        Local -> 
	  NewInfo = {ID, Loc, Pos},
	  R ! {stay_here, NewInfo, View},
	  location_loop(Loc, Neighbours, View, [NewInfo|Agents]);
	true ->
	  {Dir, V} = calculate_direction(Pos),
	  Neighbour = get_neighbour(Dir, Neighbours),
	  NewInfo = {ID, Loc, vector_sub(Pos, make_nv(V))},
	  R ! {go_there, Neighbour, NewInfo},
	  location_loop(Loc, Neighbours, View, Agents)
      end;
    {move, R, ID, Vector} ->
      {value, Info, NewAgents} = keytake(ID, 1, Agents),
      {_, _, Pos} = Info,
      NewPos = vector_add(Pos, Vector),
      Local = in_bounds(NewPos),
      if
        Local ->
	  NewInfo = {ID, Loc, NewPos},
	  R ! {stay_here, NewInfo, View},
	  location_loop(Loc, Neighbours, View, [NewInfo|NewAgents]);
	true ->
	  {Dir, V} = calculate_direction(NewPos),
	  Neighbour = get_neighbour(Dir, Neighbours),
	  NewInfo = {ID, Loc, vector_sub(NewPos, make_nv(V))},
	  R ! {go_there, Neighbour, NewInfo},
	  location_loop(Loc, Neighbours, View, NewAgents)
      end;
    {get_info, R} ->
      R ! {info, Agents},
      location_loop(Loc, Neighbours, View, Agents);
    shutdown ->
      View ! shutdown
  end.

location(Loc) ->
  receive
    {neighbours, Neighbours} ->
      Search = reverse([self()|reverse(tuple_to_list(Neighbours))]),
      View = spawn(agents, view, [Search, -1, []]),
      location_loop(Loc, Neighbours, View, [])
  end.


%% Agents

a_sqrt(_, R, 16) -> 
  R;
a_sqrt(X, R, S) ->
  NewR = (R + (X div R)) bsr 1,
  if
    NewR == R -> R;
    true      -> a_sqrt(X, NewR, S + 1)
  end.

min(A, B) ->
  if
    A < B -> A;
    true  -> B
  end.

calculate_r(DX2, DY2, R2, DX, DY) ->
  if
    DX2 > DY2 -> a_sqrt(R2, abs(DX), 0);
    DY2 > 0   -> a_sqrt(R2, abs(DY), 0);
    true      -> 1
  end.

force_vector(Cycle, DX2, DY2, R2, DX, DY, M) ->
  if
    R2 == 0 ->
      X = Cycle band 1,
      Y = (Cycle bsr 1) band 1,
      { M * (X * (-1)), M * (Y * (-1)) };
    true    ->
      R = calculate_r(DX2, DY2, R2, DX, DY),
      { (M * DX) div R, (M * DY) div R }
  end.

calculate_force(_, _, _, _, _, [], F) ->
  F;
calculate_force(Cycle, ID, PX, PY, Pos, [A|AS], F) ->
  {AgentID, _, AgentPos} = A,
  if
    AgentID == ID ->
      calculate_force(Cycle, ID, PX, PY, Pos, AS, F);
    true          ->
      {DoX, DoY}  = vector_sub(Pos, AgentPos),
      DX          = (DoX * (PX + 1)) div 128,
      DY          = (DoY * (PY + 1)) div 128,
      DX2         = DX * DX,
      DY2         = DY * DY,
      R2          = DX2 + DY2,
      M           = min(?LOC_SIZE, ((3 * ?LOC_AREA) div (R2 + 1))),
      AddF        = force_vector(Cycle, DX2, DY2, R2, DX, DY, M),
      calculate_force(Cycle, ID, PX, PY, Pos, AS, vector_add(F, AddF))
  end.

calculate_move(Cycle, {ID, _, Pos}, Persona, View) ->
  View ! {get_view, self(), Cycle},
  receive
    Agents ->
      NewPersona = Persona + length(Agents),
      PX         = NewPersona band 255,
      PY         = (NewPersona bsr 8) band 255,
      F          = calculate_force(Cycle, ID, PX, PY, Pos, Agents, {0, 0}),
      {NewPersona, F}
  end.

update_persona({_, Loc, {X, Y}}, Persona) ->
  (((((Persona band 65535) * X) band 65535) * Y) band 65535) * Loc.

enter_agent(Info, Location) ->
  Location ! {enter, self(), Info},
  receive
    {stay_here, NewInfo, View} ->
      {NewInfo, Location, View};
    {go_there, NewLocation, NewInfo} ->
      enter_agent(NewInfo, NewLocation)
  end.

move_agent({ID, _, _}, Vector, Location) ->
  Location ! {move, self(), ID, Vector},
  receive
    {stay_here, NewInfo, View} ->
      {NewInfo, Location, View};
    {go_there, NewLocation, NewInfo} ->
      enter_agent(NewInfo, NewLocation)
  end.

agent_start(Cycle, {ID, _, _}) ->
  io:fwrite("~b ~b start~n", [Cycle, ID]).
agent_at(Cycle, {ID, Loc, {X, Y}}) ->
  io:fwrite("~b ~b at ~b:~b,~b~n", [Cycle, ID, Loc, X, Y]).
agent_end(Cycle, {ID, _, _}, Persona) ->
  io:fwrite("~b ~b end ~b~n", [Cycle, ID, Persona]).

agent_loop(Cycle, Info, Persona, Location, View) ->
  if
    Cycle < ?N_CYCLES ->
      %agent_at(Cycle, Info),
      NewCycle = Cycle + 1,
      {NewPersona, Move} = calculate_move(NewCycle, Info, update_persona(Info, Persona), View),
      sync_barrier(),
      {NewInfo, NewLocation, NewView} = move_agent(Info, Move, Location),
      sync_barrier(),
      agent_loop(NewCycle, NewInfo, NewPersona, NewLocation, NewView);
    true ->
      agent_at(Cycle, Info),
      agent_end(Cycle, Info, Persona),
      sync_barrier()
  end.

agent(Cycle, Info, Location) ->
  {ID, _, _} = Info,
  NewPersona = ID * 37,
  agent_start(Cycle, Info),
  sync_barrier(),
  {NewInfo, NewLocation, NewView} = enter_agent(Info, Location),
  sync_barrier(),
  agent_loop(Cycle, NewInfo, NewPersona, NewLocation, NewView).


%% The World

wrap(N,L) ->
  if
    N < 0 -> wrap(N + L, L);
    N < L -> N;
    true  -> N rem L
  end.

spawn_world(0, W) ->
  W;
spawn_world(S, W) ->
  Location = spawn(agents, location, [S - 1]),
  spawn_world(S - 1, [Location|W]).

build_neighbours(_, _, _, [], Neighbours) ->
  list_to_tuple(reverse(Neighbours));
build_neighbours(World_Size, World, Loc, [O|OS], LS) ->
  {OX, OY} = O,
  X        = Loc rem World_Size,
  Y        = Loc div World_Size,
  NX       = wrap(X + OX, World_Size),
  NY       = wrap(Y + OY, World_Size),
  N        = (NY * World_Size) + NX,
  Location = nth(N + 1, World),
  build_neighbours(World_Size, World, Loc, OS, [Location|LS]).

add_agents(_, _, ID, 0) ->
  ID;
add_agents(Loc_Agents, Location, ID, N) ->
  P = Loc_Agents - N,
  O = ?LOC_MIN + ((?LOC_SIZE div (Loc_Agents + 4)) * (P + 2)),
  Info = {ID, -1, {O, O}},
  spawn(agents, agent, [0, Info, Location]),
  add_agents(Loc_Agents, Location, ID + 1, N - 1).

setup_world(_, _, _, _, BaseID, []) ->
  BaseID;
setup_world(WS, LA, World, Loc, BaseID, [L|LS]) ->
  [_|Offsets] = reverse(?OFFSETS),
  L ! {neighbours, build_neighbours(WS, World, Loc, reverse(Offsets), [])},
  NextID = add_agents(LA, L, BaseID, LA),
  setup_world(WS, LA, World, Loc + 1, NextID, LS).

teardown_world([]) ->
  nop;
teardown_world([L|LS]) ->
  L ! shutdown,
  teardown_world(LS).

ticker(0) ->
  sync_barrier (),
  nop;
ticker(Count) ->
  %io:fwrite("~b A~n", [Count]),
  sync_barrier (),
  %io:fwrite("~b B~n", [Count]),
  sync_barrier (),
  ticker(Count - 1).

agents(World_Size, Loc_Agents) ->
  World_Area = World_Size * World_Size,
  init_barrier((Loc_Agents * World_Area) + 1),
  World = spawn_world(World_Area, []),
  setup_world(World_Size, Loc_Agents, World, 0, 1, World),
  ticker(?N_CYCLES + 1),
  teardown_world(World),
  shutdown_barrier(),
  erlang:halt().

main() ->
  agents(?WORLD_SIZE, ?LOC_AGENTS).
main([A1|[A2|_]]) ->
  World_Size = list_to_integer(A1),
  Loc_Agents = list_to_integer(A2),
  agents(World_Size, Loc_Agents).
