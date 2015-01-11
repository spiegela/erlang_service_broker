-module(service_broker_store).

-export([init/0, insert/2, delete/2]).

-type storable_type() :: broker_instance | broker_binding.
-type storable()      :: #broker_instance{} | #broker_binding{}.

-spec init() -> ok.
init() ->
  case is_fresh_start() of
  	true             -> create_schema();
  	{exists, Tables} -> mnesia:wait_for_tables(Tables, 1000000)
  end.

-spec insert(storable_type(), storable()) -> ok.
insert(broker_instance, Inst) ->
  mnesia:transaction(fun insert_broker_instance/1, [Inst]), ok;
insert(broker_binding, Bind) ->
  mnesia:transaction(fun insert_broker_binding/1, [Bind]), ok.
  
-spec delete(storable_type(), atom()) -> ok.
delete(broker_instance, Inst) ->
  mnesia:transaction(fun delete_broker_instance/1, [Inst]), ok;
delete(broker_binding, Bind) ->
  mnesia:transaction(fun delete_broker_binding/1, [Bind]), ok.
 
-spec exists(storable_type(), storable()) -> boolean().
exists(Table, Id) ->
  case mnesia:read({Table, Id}) of [] -> false; _ -> true end.

%% @doc Check to see if mnesia has been started on this system before
is_fresh_startup() ->
  Node = node(),
  case mnesia:system_info(tables) of
  	[schema] -> true;
  	Tables   ->
  	  case mnesia:table_info(schema, cookie) of
  	  	{_, Node} -> {exists, Tables};
  	  	_         -> true
  	  end
  end.

%% @doc create initial schema for mnesia tables
create_schema() ->
  mnesia:create_schema([node()]),
  mnesia:create_table( broker_instance,
  	                   [ { attributes,
  	                       record_info(fields, broker_instance) } ] ), 
  mnesia:create_table( broker_binding,
  	                   [ { attributes,
  	                       record_info(fields, broker_binding) } ] ),
  ok.

-spec insert_broker_instance(#broker_instance{}) -> ok.
insert_broker_instance(#broker_instance{instance_id = Id}=Inst) ->
  ok = service_agent_proxy:create(Id),
  mnesia:write(Inst),
  ok.

 -spec insert_broker_binding(#broker_binding{}) -> ok.
insert_broker_binding(#broker_binding{ instance_id = Id,
	                                   app_guid = AppGuid
								     } = Bind) ->
  mnesia:write(Bind),
  ok.

-spec delete_broker_instance(#broker_instance{}) -> ok.
delete_broker_instance(#broker_instance{instance_id = Id}=Inst) ->
  ok = service_agent_proxy:delete(Id),
  mnesia:delete(Id, broker_binding),
  ok.

 -spec delete_broker_binding(#broker_binding{}) -> ok.
delete_broker_binding(#broker_binding{instance_id = Id} = Bind) ->
  mnesia:delete(Id, broker_binding),
  ok.