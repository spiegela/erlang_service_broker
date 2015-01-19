-module(service_broker_instance_handler).
-author('spiegela@gmail.com').

-export([init/2, service_available/2, allowed_methods/2,
         content_types_accepted/2, is_authorized/2, is_conflict/2,
         malformed_request/2, delete_resource/2]).

-export([put_json/2]).

-include("service_broker.hrl").

-record(state, { instance_id :: string(), body :: #broker_instance{} }).

-define(USERNAME, <<"admin">>).
-define(PASSWORD, <<"password">>).

%%% Cowboy REST Handler Callbacks

init(Req, _Opts) ->
  Id   = cowboy_req:binding(instance_id, Req),
  Id2  = list_to_atom(binary_to_list(Id)),
  % Preemptively set resp body to generic error indicator (empty JSON object)
  % This will be overwritten upon successful return or more specific error(s).
  Req1 = cowboy_req:set_resp_body("{}", Req),
  {cowboy_rest, Req1, #state{instance_id = Id2}}.

service_available(Req, State) ->
  Version = cowboy_req:header(<<"x-broker-api-version">>, Req),
  Agents  = service_agent_registry:list(),
  case {Version, Agents} of
    {<<"2.4">>, []} ->
      % No agents are available for provisioning
      {false, Req, State};
    {<<"2.4">>, Agents} ->
      % We have agents, and are responding at the right version
      {true, Req, State};
    {Version, Agents} ->
      % We have agents, but the broker API version isn't supported
      Req1 = cowboy_req:reply(412, Req), % Unsupported version return 412
      {halt, Req1, State}
  end.

is_authorized(Req, State) ->
  AuthFailure = {false, "Basic realm=\"Authenticated\""},
  case cowboy_req:header(<<"authorization">>, Req) of
  	undefined  ->
      {AuthFailure, Req, State};
  	_AuthString ->
      case cowboy_req:parse_header(<<"authorization">>, Req) of
        {<<"basic">>, {?USERNAME, ?PASSWORD}} ->
          true;
        _ ->
          {AuthFailure, Req, State}
      end
  end.

allowed_methods(Req, State) -> {[<<"PUT">>, <<"DELETE">>], Req, State}.

is_conflict(Req, #state{instance_id = Id}=State) ->
  % Return 409 Conflict if the binding already exists
  % TODO: Determine if instance is equal to return 200 OK
  {service_broker_store:exists(broker_instance, Id), Req, State}.

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

delete_resource(Req, #state{instance_id = Id }=State) ->
  service_broker_store:delete(broker_instance, Id),
  {true, Req, State}.

put_json(Req, #state{body = Body}=State) ->
  service_broker_store:insert(broker_service_instance, Body),
  {true, Req, State}.

%%% Internal Functions

%% @priv
read_body(Req, State) -> read_body(cowboy_req:method(Req), Req, State).

%% @priv
read_body(<<"PUT">>, Req, #state{instance_id = Id}) ->
  {Body, Req1} = read_chunked_body(cowboy_req:body(Req), []),
  Body1 = parse_body(Body),
  Body2 = Body1#broker_instance{instance_id = Id},
  check_body(Body2),
  {Body2, Req1};
read_body(<<"DELETE">>, Req, _State) ->
  {undefined, Req}.

%% @priv
-spec read_chunked_body({more | ok, binary(), cowboy_req:req()}, list()) ->
  {binary(), cowboy_req:req()}.
read_chunked_body({more, Data, Req}, Acc) ->
  read_chunked_body(cowboy_req:body(Req), [Data|Acc]);
read_chunked_body({ok, Data, Req}, Acc) ->
  Body = iolist_to_binary(lists:reverse([Data|Acc])),
  {Body, Req}.

%% @priv
-spec parse_body(binary()) -> #broker_instance{}.
parse_body(Body) -> {Body1} = jiffy:decode(Body), plist_to_rec(Body1).

%% @priv
-spec plist_to_rec(service_instance_list()) -> #broker_instance{}.
plist_to_rec(Plist) ->
  lists:foldl(fun add_to_record/2, #broker_instance{}, Plist).

%% @priv
-spec add_to_record(service_instance_input(), #broker_instance{}) ->
  #broker_instance{}.
add_to_record({<<"service_id">>, ServiceId}, Rec) ->
  Rec#broker_instance{service_id = ServiceId};
add_to_record({<<"plan_id">>, PlanId}, Rec) ->
  Rec#broker_instance{plan_id = PlanId};
add_to_record({<<"organization_guid">>, OrgId}, Rec) ->
  Rec#broker_instance{org_guid = OrgId};
add_to_record({<<"space_guid">>, SpaceId}, Rec) ->
  Rec#broker_instance{space_guid = SpaceId}.

-spec check_body(#broker_instance{}) -> ok.
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

-spec body_errors(#broker_instance{}) -> errors().
body_errors(Inst) ->
  lists:filtermap(fun(Field) -> body_error(Field, Inst) end, req_fields()).

-spec body_error(service_instance_field(), #broker_instance{}) ->
  false | {true, string()}.
body_error(instance_id, #broker_instance{instance_id = undefined}) ->
  {true, "Required field, instance_id, not provided."};
body_error(instance_id, #broker_instance{instance_id = Id}) ->
  case re:run(atom_to_list(Id), "^[\s|\t]*$") of
    nomatch -> false;
    _Match  -> {true, "Required field, instance_id, is blank."}
  end;
body_error(service_id, #broker_instance{service_id = undefined}) ->
  {true, "Required field, service_id, not provided."};
body_error(service_id, #broker_instance{
                         service_id = <<"9c3b67af-d80c-4e4c-89ee-f6181a3facad">>
                       }) ->
  false;
body_error(service_id, #broker_instance{service_id = _ServiceId}) ->
  {true, "Service not found in catalog"};
body_error(plan_id, #broker_instance{plan_id = undefined}) ->
  {true, "Required field, plan_id, not provided."};
body_error(plan_id, #broker_instance{
                      plan_id = <<"961dffa6-1a80-466e-8928-080f5ff9f63f">>
                    }) ->
  false;
body_error(plan_id, #broker_instance{plan_id = _PlanId}) ->
  {true, "Plan not found in catalog"};
body_error(org_guid, #broker_instance{org_guid = undefined}) ->
  {true, "Required field, org_guid, not provided."};
body_error(org_guid, #broker_instance{org_guid = OrgId}) ->
  case re:run(OrgId, "^[\s|\t]*$") of
    nomatch -> false;
    _Match  -> {true, "Required field, org_guid, is blank."}
  end;
body_error(space_guid, #broker_instance{space_guid = undefined}) ->
  {true, "Required field, space_guid, not provided."};
body_error(space_guid, #broker_instance{space_guid = SpaceId}) ->
  case re:run(SpaceId, "^[\s|\t]*$") of
    nomatch -> false;
    _Match  -> {true, "Required field, space_guid, is blank."}
  end;
body_error(_Field, _Value) ->
  false.

req_fields() -> [instance_id, plan_id, org_guid, space_guid].