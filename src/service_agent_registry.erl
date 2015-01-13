-module(service_agent_registry).

-behaviour(gen_server).

-include("service_broker.hrl").

%% API
-export([start_link/0, register/1, deregister/1, list/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {agents = sets:new() :: sets:set(agent())}).

%%% API

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec register(agent()) -> ok.
register(Address) ->
  gen_server:cast(?SERVER, {register, Address}).

-spec deregister(agent()) -> ok.
deregister(Address) ->
  gen_server:cast(?SERVER, {deregister, Address}).

-spec list() -> [agent()].
list() ->
  gen_server:call(?SERVER, list).

%%% gen_server callbacks

init([]) ->
  {ok, #state{}}.

handle_call(list, _From, #state{agents = Agents}=State) ->
  {reply, sets:to_list(Agents), State}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({register, Addr}, #state{agents = Agents}=State) ->
  {noreply, State#state{agents = sets:add_element(Addr, Agents)}};
handle_cast({deregister, Addr}, #state{agents = Agents}=State) ->
  {noreply, State#state{agents = sets:del_element(Addr, Agents)}}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% Internal functions