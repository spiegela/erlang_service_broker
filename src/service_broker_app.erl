%%% @author Aaron Spiegel <spiegela@gmail.com>
%%% @doc Main Entry point for Service Broker.
%%% @see http://github.com/spiegela/erlang-boshrelease
%%% @end

-module(service_broker_app).
-author('spiegela@gmail.com').

-behaviour(application).

-define(C_ACCEPTORS,  100).

-export([start/0, start/2, stop/0, stop/1]).

%% ADMIN API
%% @doc Starts the application
start() ->
  % application:ensure_all_started(service_broker),
  [ application:start(X) || X <- [ crypto, ranch, cowlib, cowboy, service_broker ]],
  Routes    = routes(),
  Dispatch  = cowboy_router:compile(Routes),
  Port      = port(),
  TransOpts = [{port, Port}],
  ProtoOpts = [
    {env, [ {dispatch, Dispatch} ]}
    %{onrequest, fun on_request/1}
  ],
  % Start the web-server with the appropriate options
  {ok, _} = cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
  service_broker_sup:start_link().

%% @doc Stops the application
stop() -> application:stop(service_broker).

%% BEHAVIOUR CALLBACKS
%% @private
start(_StartType, _StartArgs) -> service_broker_sup:start_link().

%% @private
stop(_State) -> ok.

port() ->
  case os:getenv("PORT") of
    false ->
        {ok, Port} = application:get_env(service_broker, http_port),
        Port;
    Other ->
      list_to_integer(Other)
  end.

routes() ->
  [
    {'_',
      [
        { "/v2/catalog", cowboy_static, {priv_file, service_broker, "catalog.json"} },
        { "/v2/service_instances/:id", service_broker_instance_handler, [] },
        { "/v2/service_bindings/:id", servirce_broker_binding_handler, [] }
      ]
    }
  ].