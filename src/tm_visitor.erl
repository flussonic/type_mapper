-module(tm_visitor).
-include("../include/type_mapper.hrl").

-export([visit/2]).


-record(tm, {
  module,
  callbacks = [],
  output
}).

visit(#{module := TModule, callbacks := Callbacks, output := Output} = Opts, Input) ->
  State = #tm{
    module = TModule,
    callbacks = Callbacks,
    output = Output
  },
  InitTypes = case Opts of
    #{type_name := TName} -> [#type_mapper_type{name=TName,source=user,body=undefined}];
    #{type := Type} -> [Type]
  end,
  case visit3(State, Input, InitTypes, undefined, []) of
    {ok, Value} -> {ok, Value};
    {error, #{} = E} -> {error, E}
  end.


or_(undefined, #{} = V) -> V;
or_(#{reason := non_matching_value}, #{} = V) -> V;
or_(#{} = V, _) -> V.


prepend(Segment, #{path := Path} = Error) -> Error#{path => [Segment|Path]};
prepend(Segment, #{} = Error) -> Error#{path => [Segment]}.



visit3(#tm{callbacks=C}=TM, Input,
  [#type_mapper_type{name = TName, source = user, body = undefined} = T|Types], LastError, Path) ->

  case process_callbacks_for_scalar(C, Input, T) of
    {ok, Input1} ->
      case visit_user_type(TM, TName, Input1, Path) of
        {ok, Value} -> {ok, Value};
        {error, #{} = E} -> visit3(TM, Input, Types, or_(LastError,prepend(TName,E)), Path)
      end;
    {error, #{} = E1} ->
      visit3(TM, Input, Types, or_(LastError,prepend(TName,E1)), Path)
  end;



% visit3(#tm{}=TM, Input, [#type_mapper_type{source = user, body = Type}|Types], LastError, Path) when Type =/= undefined ->
%   case visit3(TM, Input, [Type], LastError, Path) of
%     {error, E} ->
%       visit3(TM, Input, Types, or_(LastError, E), Path);
%     {ok, Data} ->
%       {ok, Data}
%   end;


visit3(#tm{}=TM, Input, [#type_mapper_type{name=list, body = Type}|Types], LastError, Path) ->
  case visit_list(TM, Type, Input, Path) of
    {ok, Value} -> {ok, Value};
    {error, #{} = E} -> visit3(TM, Input, Types, or_(LastError, E), Path)
  end;

visit3(#tm{}=TM, Input, [#type_mapper_type{name=record, body=RecName}|Types], LastError, Path) ->
  case visit_record(TM, RecName, Input, Path ++ [RecName]) of
    {ok, Value} -> {ok, Value};
    {error, #{} = E} -> visit3(TM, Input, Types, or_(LastError,E), Path)
  end;

visit3(#tm{}=TM, Input, [#type_mapper_type{name=map,body = MapFields}|Types], LastError, Path) ->
  Input1 = case Input of
    null -> undefined;
    _ when is_map(Input) -> maps:to_list(Input);
    _ -> Input
  end,
  case visit_map(TM, Input1, MapFields, #{}, Path) of
    {ok, Value} -> {ok, Value};
    {error, #{} = E} -> visit3(TM, Input, Types, or_(LastError,E), Path)
  end;

visit3(#tm{callbacks = C}=TM, Input, [#type_mapper_type{source=ScalarSource} = T|Types], LastError, Path) when
  ScalarSource == system orelse ScalarSource == value ->
  case process_callbacks_for_scalar(C, Input, T) of
    {ok, Value} ->
      {ok, Value};
    {error, #{} = E} ->
      visit3(TM, Input, Types, or_(LastError,E), Path)
  end;

visit3(#tm{}=TM, Input, [#type_mapper_type{name = Name, source=user,body= #type_mapper_type{} = Type}|Types], LastError, Path) ->
  case visit3(TM, Input, [Type], LastError, Path ++ [Name]) of
    {ok, Value} ->
      {ok, Value};
    {error, #{} = E} ->
      visit3(TM, Input, Types, or_(LastError,E), Path)
  end;

visit3(#tm{}, Input, [], LastError, _Path) ->
  {error, or_(LastError, #{reason => unmatched_type, detail => Input})}.



process_callbacks_for_scalar([{Module, Arg}|Callbacks], Input, Type) ->
  case Module:tm_handle_scalar(Arg, Input, Type) of
    {ok, Output} -> process_callbacks_for_scalar(Callbacks, Output, Type);
    {error, #{}} = E -> E
  end;

process_callbacks_for_scalar([], Input, _) ->
  {ok, Input}.


process_callbacks_for_close_record([{Module, Arg}|Callbacks], RecName, Input, Record) ->
  case Module:tm_handle_close_record(Arg, RecName, Input, Record) of
    {ok, Input1, Record1} -> process_callbacks_for_close_record(Callbacks, RecName, Input1, Record1);
    {error, #{}} = E -> E
  end;

process_callbacks_for_close_record([], _, _, Record) ->
  {ok, Record}.


visit_record(#tm{output=OutType, module = Module, callbacks = C}=TM, RecName, Input, Path) when 
  (is_tuple(Input) andalso element(1,Input) == RecName) orelse is_map(Input) ->
  case Module:'$mapper_record'(RecName) of
    undefined ->
      {error, #{reason => unknown_record}};
    Fields when is_list(Fields) ->
      Initial = case OutType of
        record -> {RecName};
        map -> #{}
      end,
      Input0 = case Input of
        #{} -> Input;
        _ when is_tuple(Input) -> tl(tuple_to_list(Input))
      end,
      case visit_record_fields(TM, Input0, Fields, Initial, Path) of
        {ok, Input1, Record} ->
          if 
            Input1 == #{} orelse Input1 == [] -> {ok, Record};
            true -> process_callbacks_for_close_record(C, {RecName, Fields}, Input1, Record)
          end;
        {error, #{} = E} ->
          {error, E}
      end
  end;

visit_record(#tm{callbacks = C}, RecName, Input, _) ->
  Result = process_callbacks_for_scalar(C, Input, {record, RecName}),
  Result.







visit_record_fields(#tm{output = OutType, callbacks = Callbacks}=TM, Input, 
  [#type_mapper_field{name=Name,default = DefaultValue, default_type = DefaultType,types=Types}=Field|Fields], Record, Path) ->

  {Class, ExtractedValue, Input1} = case Input of
    [Head | Tail] ->
      {input, Head, Tail};
    #{Name := V} ->
      {input, V, maps:without([Name], Input)};
    _ ->
      NameBin = atom_to_binary(Name,latin1),
      case Input of
        #{NameBin := V} ->
          {input, V, maps:without([NameBin], Input)};
        _ when DefaultType == undefined ->
          {lack, undefined, Input};
        _ when DefaultType == record ->
          {ok, PreloadedValue} = visit_record(TM, DefaultValue, #{}, Path ++ [Name]),
          {default, PreloadedValue, Input};
        _ ->
          {default, DefaultValue, Input}
      end
  end,

  ConvertedValue = case Class of
    input ->
      DefaultTypes = case DefaultType of
        undefined -> [];
        _ -> [#type_mapper_type{name=Name,source=value,body=DefaultValue}]
      end,
      case visit3(TM, ExtractedValue, Types ++ DefaultTypes, undefined, Path) of
        {ok, ExtractedValue1} ->
          {ok, ExtractedValue1};
        {error, _} when ExtractedValue == DefaultValue ->
          {ok, ExtractedValue};
        {error, E2} ->
          {error, E2}
      end;
    _ ->
      {ok, ExtractedValue}
  end,

  % io:format("~p.~p ~p ~p -> ~p\n", [Name,Class,ExtractedValue,Types,ConvertedValue]),
  NewOutput = case ConvertedValue of
    {error, E3} ->
      {error, E3};
    {ok, CV} ->
      case process_callbacks_for_record(Callbacks, Class, CV, Field) of
        {ok, Value} when OutType == map ->
          Record#{Name => Value};
        {ok, Value} when OutType == record ->
          erlang:append_element(Record, Value);
        skip when OutType == map ->
          Record;
        skip when OutType == record ->
          erlang:append_element(Record, ExtractedValue);
        {error, #{} = E} ->
          {error, E}
      end
  end,

  case NewOutput of
    {error, E1} ->
      {error, prepend(Name,E1)};
    _ ->
      visit_record_fields(TM, Input1, Fields, NewOutput, Path)
  end;

visit_record_fields(#tm{} = TM, Input, [undefined|Fields], Record, Path) ->
  visit_record_fields(TM, Input, Fields, Record, Path);

visit_record_fields(#tm{} = _TM, Input, [], Record, _Path) ->
  {ok, Input, Record}.




process_callbacks_for_record([{Module, Arg}|Callbacks], Class, ExtractedValue, Field) ->
  case Module:tm_handle_record_field(Arg, Class, ExtractedValue, Field) of
    skip -> skip;
    {ok, Output} -> process_callbacks_for_record(Callbacks, Class, Output, Field);
    {error, #{}} = E -> E;
    Else -> error({invalid_return,Module,Arg,Else})
  end;

process_callbacks_for_record([], _Class, ExtractedValue, _) ->
  {ok, ExtractedValue}.



visit_list(#tm{} = TM, Type, Input, Path) when is_list(Input) ->
  Values = lists:foldr(fun
    (_I, #{} = E) ->
      E;
    (I, List) ->
      case visit3(TM, I, [Type], undefined, Path) of
        {ok, V} -> [V|List];
        {error, #{} = E} -> E
      end
  end, [], Input),
  case Values of
    #{} = E -> {error, E};
    _ -> {ok, Values}
  end;

visit_list(#tm{callbacks = C}, Type, Input, _) ->
  Result = process_callbacks_for_scalar(C, Input, {list, Type}),
  Result.







visit_user_type(#tm{module = Module}=TM, TName, Input, Path) ->
  Result = case Module:'$mapper_type'(TName) of
    #type_mapper_type{} = Type -> visit3(TM, Input, [Type], undefined, Path);
    UserTypes when is_list(UserTypes) -> visit3(TM, Input, UserTypes, undefined, Path);
    undefined -> visit_record(TM, TName, Input, Path ++ [TName])
  end,
  Result.






visit_map(#tm{}=_TM, [], _MapFields, Acc, _Path) ->
  {ok, Acc};

visit_map(#tm{}=TM, [{K,V}|Input], MapFields, Acc, Path) ->
  case visit_map_entry(TM, K, V, MapFields, undefined, Path ++ [K], maps:size(Acc)) of
    {ok, K1, V1} ->
      visit_map(TM, Input, MapFields, Acc#{K1 => V1}, Path);
    {error, #{} = E} ->
      {error, E}
  end;

visit_map(#tm{callbacks=C}, Input, MapFields, Acc, _Path) ->
  Result = process_callbacks_for_scalar(C, Input, {map, MapFields, Acc}),
  Result.




visit_map_entry(#tm{}, K, _V, [], LastError, _, _) ->
  {error, or_(LastError, #{reason => unknown_key, detail => K})};

visit_map_entry(#tm{callbacks=C}=TM, K, V0, [{Ktype, Vtype}|MapFields], LastError, Path, SortIndex) ->
  case visit3(TM, K, [Ktype], LastError, Path) of
    {ok, K1} ->
      {ok, V} = case record_fields(TM,Vtype) of
        undefined -> {ok, V0};
        Fields -> process_callbacks_for_scalar(C, V0, {kv, #{key => K1, sort_index => SortIndex, fields => Fields}})
      end,
      case visit3(TM, V, [Vtype], LastError, Path) of
        {ok, V1} ->
          {ok, K1, V1};
        {error, #{} = ValE} ->
          case Ktype of
            #type_mapper_type{source = value, body = K} ->
              {error, prepend(K,ValE)};
            _ ->
              visit_map_entry(TM, K, V0, MapFields, or_(LastError, prepend(K,ValE)), Path, SortIndex)
          end
      end;
    {error, #{} = KeyE} ->
      visit_map_entry(TM, K, V0, MapFields, or_(LastError, prepend(K,KeyE)), Path, SortIndex)
  end.




record_fields(#tm{module=Module}=TM, #type_mapper_type{name=TName,source=user}) ->
  case Module:'$mapper_type'(TName) of
    #type_mapper_type{name = record}=T -> record_fields(TM, T);
    _ -> undefined
  end;

record_fields(#tm{module=Module}, #type_mapper_type{name=record,body=RName}) ->
  Module:'$mapper_record'(RName);

record_fields(#tm{}, #type_mapper_type{}) ->
  undefined.

