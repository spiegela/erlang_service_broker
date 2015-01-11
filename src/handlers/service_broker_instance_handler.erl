-module(service_broker_instance_handler).
-author('spiegela@gmail.com').

-export([init/2, allowed_methods/2, content_types_accepted/2, is_authorized/2,
         is_conflict/2, malformed_request/2, delete_resource/2]).

-export([put_json/2]).

-include("service_broker.hrl").

-record(state, { instance_id :: string(), body :: #broker_instance{} }).

%%% Cowboy REST Handler Callbacks

init(Req, Opts) ->
  % Preemptively set resp body to generic error indicator (empty JSON object)
  % This will be overwritten upon successful return or more specific error(s).
  Req1 = cowboy_req:set_resp_body(Req, "{}"),
  case cowboy_req:header(<<"X-Broker-Api-Version">>, Req) of
  	<<"2.4">> ->
      Id  = cowboy_req:binding(Req, instance_id),
      Id2 = list_to_atom(binary_to_list(Id)),
	  {cowboy_rest, Req1, #state{instance_id = Id2}}.
    _ ->
      Req2 = cowboy_req:reply(412, Req),  % Unsupported version return 412
      {halt, Req2, undefined}
  end.

is_authorized(Req, State) ->
  case cowboy_req:header(<<"Authentication">>, Req) of
  	undefined  ->
      {{false, "Basic realm=\"Authenticated\""}, Req, State}.
  	AuthString ->
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
  	  Req1 = cowboy_req:set_resp_body(jiffy:encode(Resp)),
  	  {true, Req1, State};
  	error:truncated_json ->
  	  Resp = #{ <<"description">> => <<"JSON request body is invalid.">> },
  	  Req1 = cowboy_req:set_resp_body(jiffy:encode(Resp)),
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
  {Body1, Req1} = read_chunked_body(cowboy_req:body(Req), []),
  Body2 = parse_body(Body1),
  Body3 = Body2#broker_instance{instance_id = Id},
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
-spec parse_body(binary()) -> #broker_instance{}.
parse_body(Body) ->
  Body   = jiffy:decode(Body),
  plist_to_rec(Body).

%% @priv
-spec plist_to_rec(service_instance_list()) -> #broker_instance{}.
plist_to_rec(Plist) ->
  lists:foldl(fun add_to_record/2, #broker_instance{}, Plist).

%% @priv
-spec add_to_record(service_attr_input(), #broker_instance{}) ->
  #broker_instance{}.
add_to_record({<<"service_id">>, ServiceId}, Rec) ->
  Rec#broker_instance{service_id = ServiceId};
add_to_record({<<"plan_id">>, PlanId}, Rec) ->
  Rec#broker_instance{plan_id = PlanId};
add_to_record({<<"organization_guid">>, OrgId}, Rec) ->
  Rec#broker_instance{org_guid = OrgId};
add_to_record({<<"space_guid">>, SpaceId}, Rec) ->
  Rec#broker_instance{space_guid = SpaceId}.

-spec check_body(#broker_instance{}) -> true | {false, Error}.
check_body(Inst) ->
  case body_errors(Inst) of [] -> ok; Errors -> throw(format_error(Errors)) end.

-spec format_errors(iolist()) -> binary().
format_errors(Errors) ->
  Errors1 = ["JSON request body is invalid:"|Errors],
  Errors2 = string:join(Errors1, ", "),
  iolist_to_binary(Errors2).

-spec body_errors(#broker_instance{}) -> errors().
body_errors(Inst) ->
  lists:filtermap(fun(Field) -> check_body(Field, Inst) end, req_fields()).

-spec body_error(service_instance_field(), #broker_instance{}) ->
  false | {true, string()}.
body_error(instance_id, #broker_instance{instance_id = undefined}, Errors) ->
  {true, "Required field, instance_id, not provided."};
body_error(instance_id, #broker_instance{instance_id = Id}, Errors) ->
  case re:run(Id, "^[\s|\t]*$") of
    nomatch -> false;
    _Match  -> {true, "Required field, instance_id, is blank."}
  end;
body_error(service_id, #broker_instance{service_id = undefined}) ->
  {true, "Required field, service_id, not provided."};
                       service_id = <<"9c3b67af-d80c-4e4c-89ee-f6181a3facad">>
                    }, Errors) ->
  false;
body_error(service_id, #broker_instance{service_id = _ServiceId}) ->
  {true, "Service not found in catalog"};
body_error(plan_id, #broker_instance{plan_id = undefined}) ->
  {true, "Required field, plan_id, not provided."};
body_error(plan_id, #broker_instance{
                       plan_id = <<"961dffa6-1a80-466e-8928-080f5ff9f63f">>
                    }, Errors) ->
  false;
body_error(plan_id, #broker_instance{plan_id = _PlanId}) ->
  {true, "Plan not found in catalog"};
body_error(org_guid, #broker_instance{org_guid = undefined}) ->
  {true, "Required field, org_guid, not provided."};
body_error(org_guid, #broker_instance{org_guid = OrgId}, Errors) ->
  case re:run(Id, "^[\s|\t]*$") of
    nomatch -> false;
    _Match  -> {true, "Required field, org_guid, is blank."}
  end;
body_error(space_guid, #broker_instance{space_guid = undefined}) ->
  {true, "Required field, space_guid, not provided."}.
body_error(space_guid, #broker_instance{space_guid = SpaceId}, Errors) ->
  case re:run(Id, "^[\s|\t]*$") of
    nomatch -> false;
    _Match  -> {true, "Required field, space_guid, is blank."}
  end;
body_error(_Field, _Value) ->
  false.

req_fields() -> [instance_id, plan_id, org_guid, space_guid].