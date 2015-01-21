%%% @author Aaron Spiegel <spiegela@gmail.com>
%%% @doc Main Entry point for Service Broker.
%%% @see http://github.com/spiegela/erlang-boshrelease
%%% @end
-module(service_broker_app).
-author('spiegela@gmail.com').

-behaviour(application).

-define(C_ACCEPTORS,  100).

-export([start/2, stop/0, stop/1, setup/0]).

%% ADMIN API
%% @doc Starts the application
start(_StartType, _StartArgs) ->
  SupReturn = service_broker_sup:start_link(),
  setup(),
  Routes    = routes(),
  Dispatch  = cowboy_router:compile(Routes),
  Port      = port(),
  TransOpts = [{port, Port}],
  ProtoOpts = [ {env, [ {dispatch, Dispatch} ]},
                {middlewares, [ cowboy_router,
                                cowboy_printer,
                                cowboy_handler
                              ]},
                {onresponse, fun cowboy_printer:execute/4}
              ],
  % Start the web-server with the appropriate options
  {ok, _} = cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
  SupReturn.

%% @doc Stops the application
stop() -> application:stop(service_broker).

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

setup() ->
  % Initialize ETS table with agent hosted instances
  service_agent_proxy:setup(),
  % Initialize Mnesia Database
  service_broker_store:init().

routes() ->
  [
    {'_',
      [
        { "/v2/catalog",
          cowboy_static,
          { priv_file,
            service_broker,
            "static/catalog.json",
            [{mimetypes, {<<"application">>, <<"json">>, []}}]
          }
        },
        { "/v2/service_instances/:instance_id",
          service_broker_instance_handler,
          []
        },
        { "/v2/service_instances/:instance_id/service_bindings/:binding_id",
          service_broker_binding_handler,
          []
        }
      ]
    }
  ].