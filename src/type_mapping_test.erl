-module(type_mapping_test).

-include("hessian.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(car, {color, model}).

type_def_registry_test() ->
    ForeignType = <<"example.Car">>,
    TypeDef = #type_def{foreign_type = ForeignType,
                        native_type = car,
                        fieldnames = record_info(fields,car)},
    Ref1 = type_mapping:locate_type_reference(TypeDef, #encoding_state{}),
    ?assertMatch( not_found, Ref1),
    {Ref2,State} = type_mapping:register_type_def(TypeDef),
    ?assertMatch( -1, Ref2),
    Ref3 = type_mapping:locate_type_reference(TypeDef, State),
    ?assertMatch( -1, Ref3),
    {Ref4, State2} = type_mapping:update_type_reference(TypeDef, State),
    ?assertMatch( 0, Ref4).

type_ref_resolution_test() ->
    EncodingState = {encoding_state,undefined,
                        [   {-1,user,<<"org.abc.A">>,
                            [username,password]},
                            {-1,resource,<<"org.abc.B">>,
                            [virtual_host,kind,name]},
                            {0,ticket,<<"org.abc.C">>,
                            [realm_name,passive_flag,active_flag,write_flag,
                            read_flag]}
                         ]
                    },
    TypeDef = type_mapping:resolve_type_ref(0, EncodingState),
    Expected = #type_def{foreign_type = <<"org.abc.C">>,
                         native_type = ticket,
                         fieldnames = [realm_name,passive_flag,active_flag,write_flag,read_flag]},
    ?assertMatch(Expected, TypeDef).
