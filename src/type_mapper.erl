-module(type_mapper).

-export([src/1, parse_transform/2]).

-export([record/3, map/3, json_schema/2]).



-record(field, {
  name,
  default,
  types
}).

-record(type, {
  name,
  source :: user | system | value | remote,
  body = []
}).

-record(state, {
  user_types = #{},
  records = #{}
}).




% $$$$$$$$\                                          
% $$  _____|                                         
% $$ |    $$$$$$\   $$$$$$\  $$$$$$\$$$$\   $$$$$$$\ 
% $$$$$\ $$  __$$\ $$  __$$\ $$  _$$  _$$\ $$  _____|
% $$  __|$$ /  $$ |$$ |  \__|$$ / $$ / $$ |\$$$$$$\  
% $$ |   $$ |  $$ |$$ |      $$ | $$ | $$ | \____$$\ 
% $$ |   \$$$$$$  |$$ |      $$ | $$ | $$ |$$$$$$$  |
% \__|    \______/ \__|      \__| \__| \__|\_______/ 


parse_transform(Forms, _Opts) ->
  #state{} = State = parse_forms(Forms),
  Forms1 = add_export_forms(Forms, State),
  Forms2 = Forms1 ++ function_forms(State),
  Forms2.


% Just a debug method
src(Path) ->
  {ok, Forms} = epp:parse_file(Path,[],[]),
  State = parse_forms(Forms),
  [io:format("T: ~p\n", [T]) || T <- maps:to_list(State#state.user_types)],
  io:format("\n"),
  [io:format("R: ~p\n", [T]) || T <- maps:to_list(State#state.records)],
  State.



add_export_forms([{attribute, Line, module, _} = Mod | Forms], #state{}) ->
  Export1 = {attribute, Line, export, [{'$mapper_record', 1}]},
  Export2 = {attribute, Line, export, [{'$mapper_type', 1}]},
  [Mod, Export1, Export2 | Forms];

add_export_forms([Attr|Forms], State) ->
  [Attr|add_export_forms(Forms, State)].


function_forms(#state{records = Records, user_types = Types}) ->
  L = 1000,

  DefClause = [{clause, L, [{var,L,'_'}], [], [
      {atom, L, undefined}
    ]}],

  RecAbst = {function, L, '$mapper_record', 1, lists:map(fun({Name,Spec}) ->
    {clause, L, [{atom, L, Name}], [], [
      erl_parse:abstract(Spec)
    ]}
  end, maps:to_list(Records)) ++ DefClause},

  TypeAbst = {function, L, '$mapper_type', 1, lists:map(fun({Name,Spec}) ->
    {clause, L, [{atom, L, Name}], [], [
      erl_parse:abstract(Spec)
    ]}
  end, maps:to_list(Types)) ++ DefClause},
  [RecAbst, TypeAbst].






parse_forms(Forms) ->
  #state{} = State = find_types(Forms, #state{}),
  State.


find_types([{attribute, _, type, Body}|Rest], #state{user_types = Types} = State) ->
  {T,Spec,Args} = Body,
  State1 = State#state{user_types = Types#{T => parse_abst_type(Spec, Args, State)}},
  find_types(Rest, State1);

find_types([{attribute, _, record, Body}|Rest], #state{records = Records} = State) ->
  {R,Spec} = Body,
  Spec1 = [cleanup_record_field(S, State) || S <- Spec],
  State1 = State#state{records = Records#{R => Spec1}},
  find_types(Rest, State1);

find_types([_|Rest], State) ->
  find_types(Rest, State);

find_types([], State) ->
  State.



parse_abst_type({type, _, union, TypeOptions}, [], #state{} = State) -> 
  [parse_abst_type(T, [], State) || T <- TypeOptions];

parse_abst_type({type, _, record, [{atom,_,Name}]}, [], #state{}) ->
  #type{name = record, source = system, body = Name};

parse_abst_type({type, _, range, [From,To]}, [], #state{} = _State) -> 
  {integer,_,F} = From,
  {integer,_,T} = To,
  #type{name = range, source = system, body = [F,T]};

parse_abst_type({type, _, map, Choices}, [], #state{} = State) ->
  Options = lists:map(fun({type, _, map_field_assoc, [Key, Value]}) ->
    {parse_abst_type(Key, [], State),
    parse_abst_type(Value, [], State)}
  end, Choices),
  #type{name = map, source = system, body = Options};

parse_abst_type({type, _, list, [T]}, [], #state{} = State) ->
  #type{name = list, source = system, body = parse_abst_type(T, [], State)};

parse_abst_type({type, _, tuple, SubTypes}, [], #state{} = State) ->
  #type{name = tuple, source = system, body = [parse_abst_type(T, [], State) || T <- SubTypes]};

parse_abst_type({type, _, TName, TBody}, [], #state{}) ->
  #type{name = TName, source = system, body = TBody};

parse_abst_type({remote_type, _, Call}, [], #state{}) ->
  [{atom,_,Module},{atom,_,Name},[]] = Call,
  #type{name = Name, source = remote, body = Module};

parse_abst_type({user_type, _, TName, []}, [], #state{}) ->
  #type{name = TName, source = user, body = undefined};

parse_abst_type({user_type, _, TName, [T]}, [], #state{} = State) ->
  #type{name = TName, source = user, body = parse_abst_type(T, [], State)};

parse_abst_type({integer, _, Value}, [], #state{}) ->
  #type{name = integer, source = value, body = Value};

parse_abst_type({atom, _, Value}, [], #state{}) ->
  #type{name = atom, source = value, body = Value};

% this is for handling
% -type deprecated_integer() :: deprecated(integer()).
parse_abst_type({var, _, Name}, [{var, _, Name}], #state{}) ->
  #type{name = identity, source = system, body = Name}.



cleanup_record_field({typed_record_field, Field, Type}, #state{} = State) ->
  Name = case Field of
    {record_field, _, {atom, _, Name_}} -> Name_;
    {record_field, _, {atom, _, Name_}, _} -> Name_
  end,

  Default = case Field of
    {record_field, _, _, {nil, _}} -> {list, []};
    {record_field, _, _, {map, _, []}} -> {map, #{}};
    {record_field, _, _, {map, _, [],_}} -> {map, #{}};
    {record_field, _, _, {ValueType, _, Value}} -> {ValueType, Value};
    {record_field, _, _, {ValueType, _, Value, _Args}} -> {ValueType, Value};
    {record_field, _, _} -> undefined
  end,

  DeunionedTypes = case parse_abst_type(Type, [], State) of
    T_ when is_tuple(T_) -> [T_];
    T_ when is_list(T_) -> T_
  end,

  #field{name = Name, default = Default, types = DeunionedTypes};

cleanup_record_field({record_field, _, _}, #state{}) ->
  undefined;

cleanup_record_field({record_field, _, _, _}, #state{}) ->
  undefined.







% $$$$$$$$\                                      $$$$$$\                                   
% \__$$  __|                                    $$  __$$\                                  
%    $$ | $$$$$$\  $$$$$$\  $$$$$$$\   $$$$$$$\ $$ /  \__|$$$$$$\   $$$$$$\  $$$$$$\$$$$\  
%    $$ |$$  __$$\ \____$$\ $$  __$$\ $$  _____|$$$$\    $$  __$$\ $$  __$$\ $$  _$$  _$$\ 
%    $$ |$$ |  \__|$$$$$$$ |$$ |  $$ |\$$$$$$\  $$  _|   $$ /  $$ |$$ |  \__|$$ / $$ / $$ |
%    $$ |$$ |     $$  __$$ |$$ |  $$ | \____$$\ $$ |     $$ |  $$ |$$ |      $$ | $$ | $$ |
%    $$ |$$ |     \$$$$$$$ |$$ |  $$ |$$$$$$$  |$$ |     \$$$$$$  |$$ |      $$ | $$ | $$ |
%    \__|\__|      \_______|\__|  \__|\_______/ \__|      \______/ \__|      \__| \__| \__|







map(Module, TName, Input) ->
  R = json2output(Module, TName, Input, map),
  R.

record(Module, TName, Input) ->
  R = json2output(Module, TName, Input, record),
  R.





json2output(Module, TName, Input, OutType) ->
  case Module:'$mapper_type'(TName) of
    #type{name = record, body = RecName} when is_map(Input) orelse is_tuple(Input)->
      case translate_record(Module, RecName, Input, OutType) of
        {error, #{} = E} ->
          {error, prepend(TName, E)};
        Record when element(1,Record) == RecName andalso OutType == record ->
          Record;
        #{} = Output when OutType == map ->
          Output
      end;
    #type{name = record} ->
      {error, prepend(TName,#{reason => non_map_input})};
    #type{name = identity} ->
      {error, prepend(TName,#{reason => unhandled_identity})};
    #type{} = Type ->
      case validate_against(Module, Input, [Type], undefined, OutType) of
        {ok, V} ->
          V;
        {error, #{} = E} ->
          {error, prepend(TName,E)}
      end;
    Types when is_list(Types) ->
      case validate_against(Module, Input, Types, undefined, OutType) of
        {ok, V} ->
          V;
        {error, #{} = E} ->
          {error, prepend(TName,E)}
      end;
    undefined ->
      {error, #{reason => unknown_type, detail => TName}}
  end.




translate_record(Module, RecName, Input, OutType) when 
  is_map(Input) orelse (is_tuple(Input) andalso element(1,Input) == RecName) ->
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
        _ -> tl(tuple_to_list(Input))
      end,
      case fill_record_fields(Module, Input0, Fields, Initial) of
        {ok, Input1, Record} when Input1 == #{} orelse Input1 == [] ->
          Record;
        {ok, Input1, _} ->
          {error, #{reason => extra_input, unparsed => Input1}};
        {error, #{} = E} ->
          {error, E}
      end
  end.




fill_record_fields(_Module, Input, [], Record) ->
  {ok, Input, Record};

fill_record_fields(Module, Input, [#field{name=Name,default = Default,types=Types}|Fields], Record) ->
  {Class, ExtractedValue, Input1} = case Input of
    [Head | Tail] ->
      {input, Head, Tail};
    #{Name := V} ->
      {input, V, maps:without([Name], Input)};
    _ ->
      NameBin = atom_to_binary(Name,latin1),
      case Input of
        #{NameBin := V} -> {input, V, maps:without([NameBin], Input)};
        _ when Default == undefined -> {lack, undefined, Input};
        _ -> 
          {_Type, DefaultValue} = Default,
          {default, DefaultValue, Input}
      end
  end,

  OutType = case Record of
    #{} -> map;
    _ -> record
  end,

  case Class of
    default ->
      NewOutput = case OutType of
        map -> Record;
        % map -> Record#{Name => ExtractedValue};
        record -> erlang:append_element(Record, ExtractedValue)
      end,
      fill_record_fields(Module, Input1, Fields, NewOutput);
    lack ->
      {error, #{path => [Name], reason => lacks_mandatory}};
    input ->
      case validate_against(Module, ExtractedValue, Types, undefined, OutType) of
        {ok, Value} ->
          NewOutput = case OutType of
            map -> Record#{Name => Value};
            record -> erlang:append_element(Record, Value)
          end,
          fill_record_fields(Module, Input1, Fields, NewOutput);
        {error, #{} = E} ->
          {error, prepend(Name, E)}
      end
  end.








validate_against(_Module, _Input, [], LastError, _) ->
  {error, or_(LastError, #{reason => unmatched_type})};

validate_against(Module, Input, [#type{name = non_neg_integer}|Types], LastError, OutType) ->
  if
    is_integer(Input) andalso Input >= 0 -> {ok, Input};
    is_integer(Input) -> validate_against(Module, Input, Types, or_(LastError, #{reason => negative_integer}), OutType);
    true -> validate_against(Module, Input, Types, or_(LastError,#{reason => non_integer}), OutType)
  end;

validate_against(Module, Input, [#type{name = integer}|Types], LastError, OutType) ->
  if
    is_integer(Input) -> {ok, Input};
    true -> validate_against(Module, Input, Types, or_(LastError,#{reason => non_integer}), OutType)
  end;

validate_against(Module, Input, [#type{name = number}|Types], LastError, OutType) ->
  if
    is_number(Input) -> {ok, Input};
    true -> validate_against(Module, Input, Types, or_(LastError,#{reason => non_number}), OutType)
  end;

validate_against(Module, Input, [#type{name = range, body = [From,To]}|Types], LastError, OutType) ->
  if
    From =< Input andalso Input =< To -> {ok, Input};
    true -> validate_against(Module, Input, Types, or_(LastError, #{reason => out_of_range}), OutType)
  end;

validate_against(Module, Input, [#type{name = binary}|Types], LastError, OutType) ->
  if
    is_binary(Input) -> {ok, Input};
    is_atom(Input) -> {ok, atom_to_binary(Input,latin1)};
    true -> validate_against(Module, Input, Types, or_(LastError,#{reason => non_binary, detail => Input}), OutType)
  end;

validate_against(Module, Input, [#type{source = value, body = ImmediateValue}|Types], LastError, OutType) ->
  if
    Input == ImmediateValue orelse 
    (Input == null andalso ImmediateValue == undefined) ->
      {ok, ImmediateValue};
    is_atom(ImmediateValue) ->
      case atom_to_binary(ImmediateValue,latin1) of
        Input -> {ok, ImmediateValue};
        _ -> validate_against(Module, Input, Types, LastError, OutType)
      end;
    true ->
      validate_against(Module, Input, Types, LastError, OutType)
  end;

validate_against(Module, Input, [#type{name = atom}|Types], LastError, OutType) ->
  if
    is_atom(Input) -> {ok, Input};
    true -> validate_against(Module, Input, Types, or_(LastError,#{reason => non_atom}), OutType)
  end;

validate_against(Module, Input, [#type{name = TName, source = user, body = undefined}|Types], LastError, OutType) ->
  case json2output(Module, TName, Input, OutType) of
    {error, E} ->
      validate_against(Module, Input, Types, or_(LastError, E), OutType);
    Data ->
      {ok, Data}
  end;

validate_against(Module, Input, [#type{source = user, body = Type}|Types], LastError, OutType) ->
  case validate_against(Module, Input, [Type], LastError, OutType) of
    {error, E} ->
      validate_against(Module, Input, Types, or_(LastError, E), OutType);
    {ok, Data} ->
      {ok, Data}
  end;

validate_against(Module, Input, [#type{name = boolean}|Types], LastError, OutType) ->
  case Input of
    true -> {ok, true};
    <<"true">> -> {ok, true};
    false -> {ok, false};
    <<"false">> -> {ok, false};
    _ -> validate_against(Module, Input, Types, or_(LastError, #{reason => non_boolean_value}), OutType)
  end;


validate_against(Module, #{} = Input, [#type{name=map, body = MapFields}|Types], LastError, OutType) ->
  case fill_map_fields(Module, maps:to_list(Input), MapFields, #{}, OutType) of
    {ok, Data} ->
      {ok, Data};
    {error, #{} = E} ->
      validate_against(Module, Input, Types, or_(LastError, E), OutType)
  end;

validate_against(Module, Input, [#type{name=map}|Types], LastError, OutType) ->
  validate_against(Module, Input, Types, or_(LastError, #{reason => non_map_value}), OutType);


validate_against(Module, Input, [#type{name=list, body = Type}|Types], LastError, OutType) when is_list(Input) ->
  Values = lists:foldr(fun
    (_I, #{} = E) ->
      E;
    (I, List) ->
      case validate_against(Module, I, [Type], undefined, OutType) of
        {ok, V} -> [V|List];
        {error, #{} = E} -> E
      end
  end, [], Input),
  case Values of
    #{} -> validate_against(Module, Input, Types, or_(LastError, Values), OutType);
    _ -> {ok, Values}
  end;

validate_against(Module, Input, [#type{name=list}|Types], LastError, OutType) ->
  validate_against(Module, Input, Types, or_(LastError, #{reason => non_list}), OutType);


validate_against(Module, Input, [#type{name=record,source=system,body=RecName}|Types], LastError, OutType) ->
  case translate_record(Module, RecName, Input, OutType) of
    {error, #{} = E} ->
      validate_against(Module, Input, Types, or_(LastError,E), OutType);
    Record when element(1,Record) == RecName andalso OutType == record ->
      {ok, Record};
    #{} = Output when OutType == map ->
      {ok, Output}
  end;

validate_against(_Module, Input, [#type{name=any}|_], _, _) ->
  {ok, Input}.



or_(undefined, #{} = V) -> V;
or_(#{} = V, _) -> V.



prepend(Segment, #{path := Path} = Error) -> Error#{path => [Segment|Path]};
prepend(Segment, #{} = Error) -> Error#{path => [Segment]}.




fill_map_fields(_Module, [], _MapFields, Acc, _OutType) ->
  {ok, Acc};

fill_map_fields(Module, [{K,V}|Input], MapFields, Acc, OutType) ->
  case fill_map_fields2(Module, K, V, MapFields, Acc, undefined, OutType) of
    {ok, Acc1} ->
      fill_map_fields(Module, Input, MapFields, Acc1, OutType);
    {error, #{} = E} ->
      {error, E}
  end.


fill_map_fields2(_Module, K, _V, [], _Acc, LastError, _) ->
  {error, or_(LastError, #{reason => unknown_key, detail => K})};

fill_map_fields2(Module, K, V, [{Ktype, Vtype}|MapFields], Acc, LastError, OutType) ->
  case validate_against(Module, K, [Ktype], LastError, OutType) of
    {ok, K1} ->
      case validate_against(Module, V, [Vtype], LastError, OutType) of
        {ok, V1} ->
          {ok, Acc#{K1 => V1}};
        {error, #{} = E} ->
          case Ktype of
            #type{source = value, body = K} ->
              {error, prepend(K,E)};
            _ ->
              fill_map_fields2(Module, K, V, MapFields, Acc, or_(LastError, prepend(K,E)), OutType)
          end
      end;
    {error, #{} = E} ->
      fill_map_fields2(Module, K, V, MapFields, Acc, or_(LastError, prepend(K,E)), OutType)
  end.





%    $$$$$\                                $$$$$$\            $$\                                         
%    \__$$ |                              $$  __$$\           $$ |                                        
%       $$ | $$$$$$$\  $$$$$$\  $$$$$$$\  $$ /  \__| $$$$$$$\ $$$$$$$\   $$$$$$\  $$$$$$\$$$$\   $$$$$$\  
%       $$ |$$  _____|$$  __$$\ $$  __$$\ \$$$$$$\  $$  _____|$$  __$$\ $$  __$$\ $$  _$$  _$$\  \____$$\ 
% $$\   $$ |\$$$$$$\  $$ /  $$ |$$ |  $$ | \____$$\ $$ /      $$ |  $$ |$$$$$$$$ |$$ / $$ / $$ | $$$$$$$ |
% $$ |  $$ | \____$$\ $$ |  $$ |$$ |  $$ |$$\   $$ |$$ |      $$ |  $$ |$$   ____|$$ | $$ | $$ |$$  __$$ |
% \$$$$$$  |$$$$$$$  |\$$$$$$  |$$ |  $$ |\$$$$$$  |\$$$$$$$\ $$ |  $$ |\$$$$$$$\ $$ | $$ | $$ |\$$$$$$$ |
%  \______/ \_______/  \______/ \__|  \__| \______/  \_______|\__|  \__| \_______|\__| \__| \__| \_______|



json_schema(Module, TName) ->
  case type2json_schema(Module, TName) of
    {error, E} ->
      {error, E};
    #{} = Schema ->
      Dependencies = fill_schema_dependencies(Module, TName, #{}),
      Schema#{
        components => #{schemas => Dependencies},
        '$schema' => <<"http://json-schema.org/schema#">>
      }
  end.



fill_schema_dependencies(Module, TName, Acc) ->
  case Module:'$mapper_type'(TName) of
    undefined ->
      case Module:'$mapper_record'(TName) of
        undefined ->
          Acc;
        _ ->
          R = fill_schema_dependencies_for_type(Module, #type{name=record,source=system,body=TName}, Acc),
          R
      end;
    #type{} = Type ->
      R = fill_schema_dependencies_for_type(Module, Type, Acc),
      case R of
        #{TName := _} -> R;
        #{} -> R#{TName => build_js_type(Type)}
      end;
    [_|_] = Types ->
      MoreSchemas = lists:foldl(fun(T, A) ->
        fill_schema_dependencies_for_type(Module, T, A)
      end, Acc, Types),
      case MoreSchemas of
        #{TName := _} -> MoreSchemas;
        #{} -> MoreSchemas#{TName => type2json_schema(Module, TName)}
      end
  end.


fill_schema_dependencies_for_type(_, #type{name = record, body = Name}, Acc) when map_get(Name, Acc) ->
  Acc;


fill_schema_dependencies_for_type(Module, #type{name = record, body = RecName}, Acc) ->
  case Module:'$mapper_record'(RecName) of
    undefined ->
      Acc;
    Fields ->
      RecordSpec = fields2json_schema(Module, Fields, #{}),
      Acc1 = Acc#{RecName => RecordSpec},

      ReferencedTypes = lists:usort(lists:flatten([Types || #field{types = Types} <- Fields])),
      Acc2 = lists:foldl(fun(T, A) ->
        fill_schema_dependencies_for_type(Module, T, A)
      end, Acc1, ReferencedTypes),
      Acc2
  end;

fill_schema_dependencies_for_type(Module, #type{name=TName,source=user,body=undefined}, Acc) ->
  Acc1 = fill_schema_dependencies(Module, TName, Acc),
  Acc1;

fill_schema_dependencies_for_type(Module, #type{source=user,body=#type{} = Type}, Acc) ->
  R = fill_schema_dependencies_for_type(Module, Type, Acc),
  R;

fill_schema_dependencies_for_type(Module, #type{name=list,body=#type{} = Type}, Acc) ->
  R = fill_schema_dependencies_for_type(Module, Type, Acc),
  R;

fill_schema_dependencies_for_type(Module, #type{name=map,body=MapBody}, Acc) ->
  R = lists:foldl(fun({Key,Value}, A) ->
    A1 = fill_schema_dependencies_for_type(Module, Key, A),
    fill_schema_dependencies_for_type(Module, Value, A1)
  end, Acc, MapBody),
  R;

fill_schema_dependencies_for_type(_, #type{}, Acc) ->
  Acc.








type2json_schema(Module, TName) ->
  case Module:'$mapper_type'(TName) of
    undefined ->
      case record2json_schema(Module, TName) of
        #{error := _} ->
          #{error => unknown_type, name => TName};
        #{} = RecordSpec ->
          RecordSpec
      end;
    % #type{name = record, body = RecName} ->
    %   record2json_schema(Module, RecName);
    #type{name = record, body = RecName} ->
      #{'$ref' => <<"#/components/schemas/",(atom_to_binary(RecName,latin1))/binary>>};
    #type{} = T ->
      build_js_type(T);
    [_|_] = Types ->
      #{anyOf => [build_js_type(T) || T <- Types]}
  end.


record2json_schema(Module, RecName) ->
  case Module:'$mapper_record'(RecName) of
    undefined ->
      #{error => unknown_record, name => RecName};
    Fields ->
      ObjectSpec = fields2json_schema(Module, Fields, #{}),
      ObjectSpec
  end.



fields2json_schema(_, [], ObjectSpec) ->
  ObjectSpec#{type => object};

fields2json_schema(Module, [#field{name=Name,default = Dfl, types=Types}|Fields], ObjectSpec) ->
  Spec1 = case Types of
    [#type{} = T] -> build_js_type(T);
    [_,_|_] -> #{anyOf => [build_js_type(T) || T <- Types]}
  end,
  Spec2 = case Dfl of
    undefined -> Spec1;
    {_,DV} when DV == #{} orelse DV == undefined orelse DV == [] -> Spec1;
    {_,DefaultValue} -> Spec1#{default => DefaultValue}
  end,
  Properties = (maps:get(properties, ObjectSpec, #{}))#{Name => Spec2},
  ObjectSpec1 = ObjectSpec#{properties => Properties},
  ObjectSpec2 = case Dfl of
    undefined ->
      Required = maps:get(required, ObjectSpec, []) ++ [Name],
      ObjectSpec1#{required => Required};
    _ ->
      ObjectSpec1
  end,
  fields2json_schema(Module, Fields, ObjectSpec2).






build_js_type(#type{name = deprecated, body = #type{} = Type}) ->
  #{} = Spec = build_js_type(Type),
  Spec#{deprecated => true};
build_js_type(#type{source = user, body = #type{} = Type}) ->
  #{} = Spec = build_js_type(Type),
  Spec;
build_js_type(#type{name = TName, source = user, body = undefined}) ->
  #{'$ref' => <<"#/components/schemas/",(atom_to_binary(TName,latin1))/binary>>};
build_js_type(#type{name = record, source = system, body = RName}) ->
  #{'$ref' => <<"#/components/schemas/",(atom_to_binary(RName,latin1))/binary>>};
build_js_type(#type{name = non_neg_integer}) -> #{type => number};
build_js_type(#type{name = integer}) -> #{type => number};
build_js_type(#type{name = number}) -> #{type => number};
build_js_type(#type{name = float}) -> #{type => number};
build_js_type(#type{name = range, body = [From,To]}) -> #{type => number, minimum => From, maximum => To};
build_js_type(#type{name = binary}) -> #{type => string};
build_js_type(#type{name = boolean}) -> #{type => boolean};
build_js_type(#type{name = list, body = B}) -> #{type => array, items => build_js_type(B)};
build_js_type(#type{name = atom,source=value,body=V}) -> #{type => string, enum => [V]};
build_js_type(#type{name = atom}) -> #{type => string};
build_js_type(#type{name = map, body = MapBody}) ->
  build_js_map(MapBody, #{
    type => object
  });
build_js_type(#type{name = any}) -> #{type => object}.



build_js_map([], Spec) ->
  Spec;
build_js_map([{#type{source=value,body=Key},Value}|MapBody], #{} = Spec) ->
  Properties = (maps:get(properties, Spec, #{}))#{Key => build_js_type(Value)},
  build_js_map(MapBody, Spec#{properties => Properties});

build_js_map([{#type{},Value}|MapBody], #{} = Spec) ->
  Properties = (maps:get(patternProperties, Spec, #{}))#{<<".*">> => build_js_type(Value)},
  build_js_map(MapBody, Spec#{patternProperties => Properties}).







