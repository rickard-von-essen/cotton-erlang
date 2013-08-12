-module(type_mapping).

-include("hessian.hrl").

-export([register_type_def/1, register_type_def/2]).
-export([resolve_type_def/3]).
-export([resolve_type_ref/2]).
-export([locate_type_reference/2]).
-export([update_type_reference/2]).
-export([count_fields/1]).

register_type_def(TypeDef) -> register_type_def(TypeDef,#encoding_state{}).

register_type_def(TypeDef, State = #encoding_state{type_defs = TypeDefs}) ->
    case locate_type_reference(TypeDef, State) of
        not_found ->
            {type_def, Native, Foreign, Fields} = TypeDef,
            NextTypeDef = {-1, Native, Foreign, Fields},
            NewState = State#encoding_state{type_defs = TypeDefs ++ [NextTypeDef]},
            {-1, NewState};
        Ref ->
            {Ref, State}
    end.

update_type_reference(TypeDef, State = #encoding_state{type_defs = TypeDefs}) ->
    NextRef = next_ref(TypeDefs),
    {type_def, Native, Foreign, Fields} = TypeDef,
    case locate_type_reference(TypeDef,State) of
        not_found ->
            NextTypeDef = {NextRef, Native, Foreign, Fields},
            NewState = State#encoding_state{type_defs = TypeDefs ++ [NextTypeDef]},
            {NextRef, NewState};
        Ref when Ref == -1 ->
            TypeDefs0 = TypeDefs -- [{Ref,Native,Foreign,Fields}],
            TypeDefs1 = TypeDefs0 ++ [{NextRef,Native,Foreign,Fields}],
            NewState = State#encoding_state{type_defs = TypeDefs1},
            {NextRef, NewState};
        Ref ->
            {Ref, State}
    end.

locate_type_reference(TypeDef = #type_def{native_type = NativeType},
                      State = #encoding_state{type_defs = TypeDefs})
                      when not (NativeType == undefined) ->
    case lists:keysearch(NativeType, 2, TypeDefs) of
        {value, {Ref,Native,Foreign,FieldNames} } ->
            Ref;
        _ ->
            not_found
    end;

locate_type_reference(TypeDef = #type_def{native_type = NativeType,
                                          foreign_type = ForeignType},
                          State = #encoding_state{type_defs = TypeDefs})
                          when not (ForeignType == undefined) ->
    case lists:keysearch(ForeignType, 3, TypeDefs) of
        {value, {Ref,Native,Foreign,FieldNames} } ->
            Ref;
        _ ->
            not_found
    end;

locate_type_reference(TypeDef,State = #encoding_state{type_defs = []}) -> not_found.

resolve_type_def(Any,Type,#encoding_state{type_defs = []}) -> not_found;

resolve_type_def(foreign, NativeType,
                 #encoding_state{type_defs = TypeDefs}) ->
    case lists:keysearch(NativeType, 2, TypeDefs) of
        {value, {Ref,Native,Foreign,FieldNames} } ->
            #type_def{native_type = Native,
                      foreign_type = Foreign,
                      fieldnames = FieldNames};
        _ ->
            not_found
    end;

resolve_type_def(native, ForeignType,
                 #encoding_state{type_defs = TypeDefs}) ->
    case lists:keysearch(ForeignType, 3, TypeDefs) of
        {value, {Ref,Native,Foreign,FieldNames} } ->
            #type_def{native_type = Native,
                      foreign_type = Foreign,
                      fieldnames = FieldNames};
        _ ->
            not_found
    end.

next_ref([]) -> 0;
next_ref(List) ->
    {Max,_,_,_} = lists:max(List),
    Max + 1.

%% TODO This unresolvable_type_def should manifest itself in a protocol error
resolve_type_ref(Ref, #encoding_state{type_defs = []}) -> throw(unresolvable_type_def);
resolve_type_ref(Ref, #encoding_state{type_defs = TypeDefs}) ->
    Filtered = lists:filter(fun is_referenced/1, TypeDefs),
    {_,Native,Foreign,FieldNames} = lists:nth(Ref + 1, Filtered),
    #type_def{native_type = Native,
              foreign_type = Foreign,
              fieldnames = FieldNames}.

is_referenced({-1,Native,Foreign,FieldNames}) -> false;
is_referenced({_,Native,Foreign,FieldNames}) -> true.

count_fields(#type_def{fieldnames = FieldNames}) -> length(FieldNames).
