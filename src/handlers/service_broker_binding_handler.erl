-module(service_broker_binding_handler).
-author('spiegela@gmail.com').

-export([init/2, service_available/2, allowed_methods/2,
         content_types_accepted/2, is_authorized/2, resource_exists/2,
         is_conflict/2, malformed_request/2, delete_resource/2]).

-export([put_json/2]).

-include_lib("service_agent/include/service_agent.hrl").
-include("service_broker.hrl").

-record(state, { instance_id :: string(),
                 binding_id  :: string(),
                 body :: #broker_binding{}
               } ).

-define(USERNAME, <<"admin">>).
-define(PASSWORD, <<"password">>).

%%% Cowboy REST Handler Callbacks

init(Req, _Opts) ->
  InstId  = cowboy_req:binding(instance_id, Req),
  InstId2 = list_to_atom(binary_to_list(InstId)),
  BindId  = cowboy_req:binding(binding_id, Req),
  BindId2 = list_to_atom(binary_to_list(BindId)),
  % Preemptively set resp body to generic error indicator (empty JSON object)
  % This will be overwritten upon successful return or more specific error(s).
  Req1    = cowboy_req:set_resp_body("{}", Req),
  {cowboy_rest, Req1, #state{instance_id = InstId2, binding_id = BindId2}}.

service_available(Req, State) ->
  case cowboy_req:header(<<"X-Broker-Api-Version">>, Req) of
    <<"2.4">> ->
      {true, Req, State};
    _ ->
      Req1 = cowboy_req:reply(412, Req), % Unsupported version return 412
      {halt, Req1, State}
  end.

is_authorized(Req, State) ->
  AuthFailure = {false, "Basic realm=\"Authenticated\""},
  case cowboy_req:header(<<"Authentication">>, Req) of
    undefined  ->
      {AuthFailure, Req, State};
    _AuthString ->
      case cowboy_req:parse_header(<<"Authentication">>, Req) of
        {?USERNAME, ?PASSWORD} -> true;
        _                      -> {AuthFailure, Req, State}
      end
  end.

allowed_methods(Req, State) -> {[<<"PUT">>, <<"DELETE">>], Req, State}.

resource_exists(Req, #broker_binding{instance_id = Id}=State) ->
  % Return 404 if the instance doesn't exist
  {service_broker_store:exists(broker_instance, Id), Req, State}.

is_conflict(Req, #state{binding_id = Id}=State) ->
  % Return 409 Conflict if the binding already exists
  % TODO: Determine if binding is equal to return 200 OK
  {service_broker_store:exists(broker_binding, Id), Req, State}.

content_types_accepted(Req, State) ->
  { [ { {<<"application">>, <<"json">>, '*'}, put_json } ], Req, State}.

malformed_request(Req, State) ->
  try read_body(Req, State) of
    {Body, Req1} -> {false, Req1, State#state{body = Body}}
  catch
  	throw:Errors ->
      Resp = #{ <<"description">> => Errors },
  	  Req1 = cowboy_req:set_resp_body(jiffy:encode(Resp), Req),
  	  {true, Req1, State};
  	error:truncated_json ->
  	  Resp = #{ <<"description">> => <<"JSON request body is invalid.">> },
  	  Req1 = cowboy_req:set_resp_body(jiffy:encode(Resp), Req),
  	  {true, Req1, State}
  end.

delete_resource(Req, #state{binding_id = Id}=State) ->
  service_broker_store:delete(broker_binding, Id),
  {true, Req, State}.

put_json(Req, #state{body = Body, instance_id = Id}=State) ->
  service_broker_store:insert(broker_service_binding, Body),
  Body = agent_instance_creds(service_agent_proxy:get(Id)),
  Req1 = cowboy_req:set_resp_body(jiffy:encode(Body), Req),
  {true, Req1, State}.

%%% Internal Functions

%% @priv
read_body(Req, State) -> read_body(cowboy_req:method(Req), Req, State).

%% @priv
read_body(<<"PUT">>, Req, #state{ instance_id = InstId,
                                  binding_id = BindId
                                }) ->
  {Body1, Req1} = read_chunked_body(cowboy_req:body(Req), []),
  Body2 = parse_body(Body1),
  Body3 = Body2#broker_binding{ instance_id = InstId, binding_id = BindId},
  check_body(Body3),
  {Body3, Req1};
read_body(<<"DELETE">>, Req, _State) ->
  {undefined, Req}.

%% @priv
read_chunked_body({more, Data, Req}, Acc) ->
  read_chunked_body(cowboy_req:body(Req), [Data|Acc]);
read_chunked_body({ok, Data, Req}, Acc) ->
  Body = iolist_to_binary(lists:reverse([Data|Acc])),
  {Body, Req}.

%% @priv
-spec parse_body(binary()) -> #broker_binding{}.
parse_body(Body) -> Body1 = jiffy:decode(Body), plist_to_rec(Body1).

%% @priv
-spec plist_to_rec(service_binding_list()) -> #broker_binding{}.
plist_to_rec(Plist) ->
  lists:foldl(fun add_to_record/2, #broker_binding{}, Plist).

%% @priv
-spec add_to_record(service_binding_input(), #broker_binding{}) ->
  #broker_binding{}.
add_to_record({<<"service_id">>, ServiceId}, Rec) ->
  Rec#broker_binding{service_id = ServiceId};
add_to_record({<<"plan_id">>, PlanId}, Rec) ->
  Rec#broker_binding{plan_id = PlanId};
add_to_record({<<"app_guid">>, AppId}, Rec) ->
  Rec#broker_binding{app_guid = AppId}.

-spec check_body(#broker_binding{}) -> ok.
check_body(Inst) ->
  case body_errors(Inst) of
    [] -> ok;
    Errors -> throw(format_errors(Errors))
  end.

-spec format_errors(iolist()) -> binary().
format_errors(Errors) ->
  Errors1 = ["JSON request body is invalid:"|Errors],
  Errors2 = string:join(Errors1, ", "),
  iolist_to_binary(Errors2).

-spec body_errors(#broker_binding{}) -> errors().
body_errors(Inst) ->
  lists:filtermap(fun(Field) -> body_error(Field, Inst) end, req_fields()).

-spec body_error(service_binding_field(), #broker_binding{}) ->
  false | {true, string()}.
body_error(instance_id, #broker_binding{instance_id = undefined}) ->
  {true, "Required field, instance_id, not provided."};
body_error(instance_id, #broker_binding{instance_id = Id}) ->
  case re:run(Id, "^[\s|\t]*$") of
    nomatch -> false;
    _Match  -> {true, "Required field, instance_id, is blank."}
  end;
body_error(binding_id, #broker_binding{binding_id = undefined}) ->
  {true, "Required field, binding_id, not provided."};
body_error(binding_id, #broker_binding{binding_id = Id}) ->
  case re:run(Id, "^[\s|\t]*$") of
    nomatch -> false;
    _Match  -> {true, "Required field, binding_id, is blank."}
  end;
body_error(service_id, #broker_binding{service_id = undefined}) ->
  {true, "Required field, service_id, not provided."};
body_error(service_id, #broker_binding{
                         service_id = <<"9c3b67af-d80c-4e4c-89ee-f6181a3facad">>
                       }) ->
  false;
body_error(service_id, #broker_binding{service_id = _ServiceId}) ->
  {true, "Service not found in catalog"};
body_error(plan_id, #broker_binding{plan_id = undefined}) ->
  {true, "Required field, plan_id, not provided."};
body_error(plan_id, #broker_binding{
                       plan_id = <<"961dffa6-1a80-466e-8928-080f5ff9f63f">>
                    }) ->
  false;
body_error(app_guid, #broker_binding{app_guid = undefined}) ->
  {true, "Required field, app_guid, not provided."};
body_error(_Field, _Inst) ->
  false.

req_fields() -> [instance_id, binding_id, service_id, plan_id, app_guid].

-spec agent_instance_creds(#agent_instance{}) -> map().
agent_instance_creds(#agent_instance{ instance_id = InstId,
                                      cookie = Cookie,
                                      bind_addr = BindAddr,
                                      dist_min = DistMin,
                                      dist_max = DistMax
                                    }) ->
  #{<<"credentials">> => #{
       <<"instance_id">> => list_to_binary(atom_to_list(InstId)),
       <<"bind_addr">>   => list_to_binary(BindAddr),
       <<"cookie">>      => list_to_binary(atom_to_list(Cookie)),
       <<"dist_min">>    => integer_to_binary(DistMin),
       <<"dist_max">>    => integer_to_binary(DistMax)
    }
  }.