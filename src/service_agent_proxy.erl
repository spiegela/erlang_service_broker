%%% @author Aaron Spiegel <spiegela@gmail.com>
%%% @doc Module to encapsulate agent selection logic for the broker.
%%% @end
-module(service_agent_proxy).

-export([setup/0, create/1, create/2, create/3, create/4, delete/1, delete/2,
         refresh_instances/0, get/1]).

-include_lib("service_agent/include/service_agent.hrl").
-include("service_broker.hrl").

-define(DIST_PORT_START, 9100).
-define(DIST_RANGE_SIZE, 16).

-type instance_id() :: atom().
-type instance_loc() :: {agent(), #agent_instance{}}.
-type agent_load() :: {non_neg_integer(), agent()}.
-type dist_min() :: pos_integer().
-type cookie() :: atom().

%%% Public Functions

%% @doc Create a new instance on an best available agent
-spec create(instance_id()) -> ok.
create(Id) ->
  create(Id, next_agent()).

%% @doc Create a new instance on a specific agent
-spec create(instance_id(), agent()) -> ok.
create(Id, Agent) -> 
  create(Id, Agent, next_dist_min(Agent)).

%% @doc Create a new instance on a specific agent and dist_min
-spec create(instance_id(), agent(), pos_integer()) -> ok.
create(Id, Agent, DistMin) ->
  create(Id, Agent, DistMin, generate_cookie()).

%% @doc Create a new instance on a specific agent, dist_min and cookie
-spec create(instance_id(), agent(), dist_min(), cookie()) -> ok.
create(Id, Agent, DistMin, Cookie) ->
  DistMax = DistMin - 1 + ?DIST_RANGE_SIZE,
  Inst = #agent_instance{ instance_id = Id,
                          cookie      = Cookie,
                          dist_min    = DistMin,
                          dist_max    = DistMax
                         },
  ets:insert(instance_locations, {Agent, Inst}),
  rpc:call(Agent, service_agent, create, [Inst]).

-spec delete(instance_id()) -> ok.
delete(Id) ->
  {Agent, Inst} = Location = location_by_id(Id),
  rpc:call(Agent, service_agent, delete, [Inst]),
  ets:delete_object(instance_locations, Location).

%% @doc (Re)populate the instance list.
-spec refresh_instances() -> ok.
refresh_instances() ->
  ets:delete_all_objects(instance_locations),
  case query_instances() of
    []       -> ok;
    InstList -> true = ets:insert(instance_locations, InstList), ok
  end.

%% @doc initialize tables for setup
-spec setup() -> ok.
setup() ->
  instance_locations = ets:new(instance_locations, [set, named_table, public]),
  refresh_instances().

%%% Internal Functions

%% @doc Select the preferrable agent to provision from.
-spec next_agent() -> agent().
next_agent() ->
  Counters = lists:map(fun(A) -> {0, A} end, agents()),
  Loads = ets:foldl(fun agent_load/2, Counters, instance_locations),
  {_Load, Agent} = lists:min(Loads),
  Agent.

%% @doc Select the agent containing a provided id.
-spec agent_by_id(instance_id()) -> agent().
agent_by_id(Id) ->
  Match = {'$1', #agent_instance{instance_id = Id, _ = '_'}},
  [[Value]] = ets:match(instance_locations, Match), Value.

%% @doc Select the location of a provided id.
-spec location_by_id(instance_id()) -> #agent_instance{}.
location_by_id(Id) ->
  Match = {'_', #agent_instance{instance_id = Id, _ = '_'}},
  [Location] = ets:match_object(instance_locations, Match), Location.

-spec next_dist_min(agent()) -> pos_integer().
next_dist_min(Agent) ->
  Match = {Agent, #agent_instance{dist_min = '$1', _ = '_'}},
  Ports = lists:sort(lists:flatten(ets:match(instance_locations, Match))),
  next_avail_port(Ports).

-spec next_avail_port([pos_integer()]) -> pos_integer().
next_avail_port([])    -> ?DIST_PORT_START;
next_avail_port(Ports) -> next_avail_port(Ports, ?DIST_PORT_START).

-spec next_avail_port([pos_integer()], pos_integer()) -> pos_integer().
next_avail_port([], UnusedPort) ->
  UnusedPort;
next_avail_port([UsedPort|T], UsedPort) ->
  next_avail_port(T, UsedPort + ?DIST_RANGE_SIZE);
next_avail_port([UsedPort|_T], UnusedPort) when UnusedPort > UsedPort ->
  UnusedPort.

-spec generate_cookie() -> atom().
generate_cookie() -> list_to_atom(binary_to_list(ossp_uuid:make(v4, text))).

-spec agent_load(instance_loc(), [agent_load()]) -> [agent_load()].
agent_load({Agent, _Inst}, Loads) ->
  case lists:keyfind(Agent, 2, Loads) of
  	false         -> Loads;
  	{Load, Agent} -> lists:keystore(Agent, 2, Loads, {Load + 1, Agent})
  end.

-spec query_instances() -> [instance_loc()].
query_instances() -> lists:flatmap(fun query_instances/1, agents()).

-spec query_instances(agent()) -> [instance_loc()].
query_instances(Agent) ->
  [{Agent, Inst} || Inst <- rpc:call(Agent, service_agent, list, [])].

-spec agents() -> [agent()].
agents() -> service_agent_registry:list().

-spec host_to_agent(string()) -> agent().
host_to_agent(Host) -> list_to_atom("service_agent@" ++ Host).