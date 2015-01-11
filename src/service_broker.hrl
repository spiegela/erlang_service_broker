-type service_instance_list()  :: [service_instance_input()].
-type service_binding_list()   :: [service_binding_input()].
-type service_instance_input() :: {  <<"plan_id">> |
                                       <<"service_id">> |
                                       <<"organization_guid">> |
                                       <<"space_guid">>,
                                     binary()
  							                  }.
-type service_binding_input() :: {  <<"plan_id">> |
                                       <<"service_id">> |
                                       <<"app_guid">>,
                                     binary()
                                  }.
-type service_instance_field() :: instance_id | plan_id | org_guid | space_guid.
-type service_binding_field()  :: instance_id | plan_id | app_guid.
-type errors() :: iolist().

-record(broker_instance, {
    instance_id :: atom(),
    service_id  :: binary(),
    plan_id     :: binary(),
    org_guid    :: binary(),
    space_guid  :: binary()
  }).

-record(broker_binding, {
    instance_id :: atom(),
    binding_id  :: atom(),
    plan_id     :: binary(),
    service_id  :: binary(),
    app_guid    :: binary()
  }).