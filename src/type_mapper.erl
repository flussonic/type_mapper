-module(type_mapper).
-include("../include/type_mapper.hrl").
-export([src/1, parse_transform/2]).

-export([record/3, record/4, map/3, map/4, json_schema/2]).


-define(IS_TRIVIAL(X), (X == #{} orelse X == undefined orelse X == [])).

-record(state, {
  user_types = #{},
  records = #{}
}).


-define(SKIP_MAP_DEFAULTS, trivial).
-define(ALLOW_FORCED_UNDEFINED, false).
-define(ALLOW_TYPE_CONVERTION, false).
-define(ALLOW_MISS_MANDATORY, false).
-define(FILL_AUTO_FIELDS, true).

-record(mapper, {
  module :: atom(),
  output :: map | record,
  skip_map_defaults = ?SKIP_MAP_DEFAULTS :: boolean()|trivial,
  allow_forced_undefined = ?ALLOW_FORCED_UNDEFINED :: boolean(),
  allow_type_convertion = ?ALLOW_TYPE_CONVERTION :: boolean(),
  allow_miss_mandatory = ?ALLOW_MISS_MANDATORY :: boolean(),
  fill_auto_fields = ?FILL_AUTO_FIELDS :: boolean()
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
  Export3 = {attribute, Line, export, [{'$mapper_records', 0}]},
  [Mod, Export1, Export2, Export3 | Forms];

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

  RecordsAbst = {function, L, '$mapper_records', 0, [
    {clause, L, [], [], [erl_parse:abstract(maps:keys(Records))]}
  ]},

  TypeAbst = {function, L, '$mapper_type', 1, lists:map(fun({Name,Spec}) ->
    {clause, L, [{atom, L, Name}], [], [
      erl_parse:abstract(Spec)
    ]}
  end, maps:to_list(Types)) ++ DefClause},
  [RecAbst, RecordsAbst, TypeAbst].






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
  Spec2 = lists:zipwith(fun
    (#type_mapper_field{} = F, I) -> F#type_mapper_field{index = I+1};
    (undefined, _) -> undefined
  end, Spec1, lists:seq(1,length(Spec1))),
  State1 = State#state{records = Records#{R => Spec2}},
  find_types(Rest, State1);

find_types([_|Rest], State) ->
  find_types(Rest, State);

find_types([], State) ->
  State.



parse_abst_type({type, _, union, TypeOptions}, [], #state{} = State) -> 
  [parse_abst_type(T, [], State) || T <- TypeOptions];

parse_abst_type({type, _, record, [{atom,_,Name}]}, [], #state{}) ->
  #type_mapper_type{name = record, source = system, body = Name};

parse_abst_type({type, _, range, [From,To]}, [], #state{} = _State) -> 
  {integer,_,F} = From,
  {integer,_,T} = To,
  #type_mapper_type{name = range, source = system, body = [F,T]};

parse_abst_type({type, _, map, Choices}, [], #state{} = State) ->
  Options = lists:map(fun({type, _, map_field_assoc, [Key, Value]}) ->
    {parse_abst_type(Key, [], State),
    parse_abst_type(Value, [], State)}
  end, Choices),
  #type_mapper_type{name = map, source = system, body = Options};

parse_abst_type({type, _, list, [T]}, [], #state{} = State) ->
  #type_mapper_type{name = list, source = system, body = parse_abst_type(T, [], State)};

parse_abst_type({type, _, tuple, SubTypes}, [], #state{} = State) ->
  #type_mapper_type{name = tuple, source = system, body = [parse_abst_type(T, [], State) || T <- SubTypes]};

parse_abst_type({type, _, TName, TBody}, [], #state{}) ->
  #type_mapper_type{name = TName, source = system, body = TBody};

parse_abst_type({remote_type, _, Call}, [], #state{}) ->
  [{atom,_,Module},{atom,_,Name},[]] = Call,
  #type_mapper_type{name = Name, source = remote, body = Module};

parse_abst_type({user_type, _, TName, []}, [], #state{}) ->
  #type_mapper_type{name = TName, source = user, body = undefined};

parse_abst_type({user_type, _, TName, [T]}, [], #state{} = State) ->
  #type_mapper_type{name = TName, source = user, body = parse_abst_type(T, [], State)};

parse_abst_type({integer, _, Value}, [], #state{}) ->
  #type_mapper_type{name = integer, source = value, body = Value};

parse_abst_type({atom, _, Value}, [], #state{}) ->
  #type_mapper_type{name = atom, source = value, body = Value};

% this is for handling
% -type deprecated_integer() :: deprecated(integer()).
parse_abst_type({var, _, Name}, [{var, _, Name}], #state{}) ->
  #type_mapper_type{name = identity, source = system, body = Name}.



cleanup_record_field({typed_record_field, Field, Type}, #state{} = State) ->
  Name = case Field of
    {record_field, _, {atom, _, Name_}} -> Name_;
    {record_field, _, {atom, _, Name_}, _} -> Name_
  end,

  {DefaultType,Default} = case Field of
    {record_field, _, _, {nil, _}} -> {list, []};
    {record_field, _, _, {map, _, []}} -> {map, #{}};
    {record_field, _, _, {map, _, [],_}} -> {map, #{}};
    {record_field, _, _, {ValueType, _, Value}} -> {ValueType, Value};
    {record_field, _, _, {ValueType, _, Value, _Args}} -> {ValueType, Value};
    {record_field, _, _} -> {undefined,undefined}
  end,

  DeunionedTypes = case parse_abst_type(Type, [], State) of
    T_ when is_tuple(T_) -> [T_];
    T_ when is_list(T_) -> T_
  end,

  #type_mapper_field{name = Name, default = Default, default_type = DefaultType, types = DeunionedTypes};

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
  map(Module, TName, Input, #{}).

map(Module, TName, Input, #{} = Options) ->
  R = json2output0(Options#{output => map, module => Module}, TName, Input),
  R.


record(Module, TName, Input) ->
  record(Module, TName, Input, #{}).

record(Module, TName, Input, #{} = Options) ->
  R = json2output0(Options#{output => record, module => Module}, TName, Input),
  R.



json2output0(#{module := Module, output := OutType} = Opts, TName, Input) ->
  State = #mapper{
    module = Module,
    output = OutType,
    skip_map_defaults = maps:get(skip_map_defaults, Opts, ?SKIP_MAP_DEFAULTS),
    allow_forced_undefined = maps:get(allow_forced_undefined, Opts, ?ALLOW_FORCED_UNDEFINED),
    allow_type_convertion = maps:get(allow_type_convertion, Opts, ?ALLOW_TYPE_CONVERTION),
    allow_miss_mandatory = maps:get(allow_miss_mandatory, Opts, ?ALLOW_MISS_MANDATORY),
    fill_auto_fields = maps:get(fill_auto_fields, Opts, ?FILL_AUTO_FIELDS)
  },
  json2output(State, TName, Input).


json2output(#mapper{module=Module, output = OutType} = M, TName, Input) ->
  case Module:'$mapper_type'(TName) of
    #type_mapper_type{name = record, body = RecName} when is_map(Input) orelse is_tuple(Input)->
      case translate_record(M, RecName, Input) of
        {error, #{} = E} ->
          {error, prepend(TName, E)};
        Record when element(1,Record) == RecName andalso OutType == record ->
          Record;
        #{} = Output when OutType == map ->
          Output
      end;
    #type_mapper_type{name = record} when (Input == undefined orelse Input == null) andalso M#mapper.allow_forced_undefined ->
      undefined;
    #type_mapper_type{name = record} ->
      {error, prepend(TName,#{reason => non_map_input})};
    #type_mapper_type{name = identity} ->
      {error, prepend(TName,#{reason => unhandled_identity})};
    #type_mapper_type{} = Type ->
      case validate_against(M, Input, [Type], undefined) of
        {ok, V} ->
          V;
        skip ->
          undefined;
        {error, #{} = E} ->
          {error, prepend(TName,E)}
      end;
    Types when is_list(Types) ->
      case validate_against(M, Input, Types, undefined) of
        {ok, V} ->
          V;
        skip ->
          undefined;
        {error, #{} = E} ->
          {error, prepend(TName,E)}
      end;
    undefined ->
      case translate_record(M, TName, Input) of
        {error, #{reason := unknown_record}} ->
          {error, #{reason => unknown_type, detail => TName}};
        {error, E} ->
          {error, E};
        Record when element(1,Record) == TName andalso OutType == record ->
          Record;
        #{} = Output when OutType == map ->
          Output
      end
  end.



translate_record(#mapper{module=Module,output=OutType} = M, RecName, Input) when 
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
      case fill_record_fields(M, Input0, Fields, Initial) of
        {ok, Input1, Record} ->
          if 
            Input1 == #{} orelse Input1 == [] -> Record;
            true -> {error, #{reason => extra_input, unparsed => Input1}}
          end;
        {error, #{} = E} ->
          {error, E}
      end
  end;

translate_record(_M, RecName, _Input) ->
  {error, #{reason => scalar_input, record => RecName}}.




fill_record_fields(#mapper{}, Input, [], Record) ->
  {ok, Input, Record};

fill_record_fields(#mapper{output = OutType}=M, #{'$reset' := true} = Input, Types, Record) ->
  case OutType of
    map -> fill_record_fields(M, maps:remove('$reset', Input), Types, Record#{'$reset' => true});
    record -> fill_record_fields(M, maps:remove('$reset', Input), Types, Record)
  end;

fill_record_fields(#mapper{} = M, Input, [undefined|Fields], Record) ->
  fill_record_fields(M, Input, Fields, Record);

fill_record_fields(#mapper{output = OutType}=M, Input, [#type_mapper_field{name=Name,default = DefaultValue,
  default_type = DefaultType,types=Types}=_T|Fields], Record) ->

  {Class, ExtractedValue, Input1} = case Input of
    [Head | Tail] ->
      {input, Head, Tail};
    #{Name := null} when M#mapper.allow_type_convertion ->
      {input, undefined, maps:without([Name], Input)};
    #{Name := V} ->
      {input, V, maps:without([Name], Input)};
    _ ->
      NameBin = atom_to_binary(Name,latin1),
      case Input of
        #{NameBin := null} -> {input, undefined, maps:without([NameBin], Input)};
        #{NameBin := V} -> {input, V, maps:without([NameBin], Input)};
        _ when DefaultType == undefined -> {lack, undefined, Input};
        _ when DefaultType == record -> {default, translate_record(M, DefaultValue, #{}), Input};
        _ -> {default, DefaultValue, Input}
      end
  end,

  if
    Class == default orelse (Class == input andalso ExtractedValue == DefaultValue) orelse
    (Class == lack andalso M#mapper.allow_miss_mandatory) ->
      NewOutput = case OutType of
        map when Class == input -> Record#{Name => ExtractedValue};
        map when M#mapper.skip_map_defaults == true andalso Class == default -> Record;
        map when M#mapper.skip_map_defaults == trivial andalso Class == default andalso 
          (ExtractedValue == [] orelse ExtractedValue == #{}) -> Record;
        map when DefaultValue == undefined -> Record;
        map -> Record#{Name => ExtractedValue};
        record -> erlang:append_element(Record, ExtractedValue)
      end,
      fill_record_fields(M, Input1, Fields, NewOutput);
    Class == lack ->
      {error, #{path => [Name], reason => lacks_mandatory}};
    Class == input ->
      case validate_against(M, ExtractedValue, Types, undefined) of
        {ok, Value} ->
          NewOutput = case OutType of
            map -> Record#{Name => Value};
            record -> erlang:append_element(Record, Value)
          end,
          fill_record_fields(M, Input1, Fields, NewOutput);
        skip ->
          fill_record_fields(M, Input1, Fields, Record);
        {error, #{} = E} ->
          {error, prepend(Name, E)}
      end
  end.






validate_against(#mapper{allow_forced_undefined = true}, undefined, _, _) ->
  {ok, undefined};

validate_against(#mapper{allow_forced_undefined = true}, null, _, _) ->
  {ok, undefined};

validate_against(#mapper{}, _Input, [], LastError) ->
  {error, or_(LastError, #{reason => unmatched_type})};

validate_against(#mapper{allow_type_convertion = true}=M, Input,
  [#type_mapper_type{name=IntType}=T|Types], LastError) when is_binary(Input) andalso 
  (IntType == non_neg_integer orelse IntType == integer orelse IntType == number orelse IntType == range) ->
  case string:to_integer(Input) of
    {IntValue, <<>>} ->
      case validate_against(M, IntValue, [T], undefined) of
        {ok, Value} -> {ok, Value};
        {error, E} -> validate_against(M, Input, Types, or_(LastError, E))
      end;
    _ ->
      validate_against(M, Input, Types, or_(LastError, #{reason => non_integer, detail => Input, notice => autoconvertion_failed}))
  end;


validate_against(#mapper{}=M, Input, [#type_mapper_type{name = non_neg_integer}|Types], LastError) ->
  if
    is_integer(Input) andalso Input >= 0 -> {ok, Input};
    is_integer(Input) -> validate_against(M, Input, Types, or_(LastError, #{reason => negative_integer}));
    true -> validate_against(M, Input, Types, or_(LastError,#{reason => non_integer}))
  end;

validate_against(#mapper{}=M, Input, [#type_mapper_type{name = integer}|Types], LastError) ->
  if
    is_integer(Input) -> {ok, Input};
    true -> validate_against(M, Input, Types, or_(LastError,#{reason => non_integer}))
  end;

validate_against(#mapper{}=M, Input, [#type_mapper_type{name = number}|Types], LastError) ->
  if
    is_number(Input) -> {ok, Input};
    true -> validate_against(M, Input, Types, or_(LastError,#{reason => non_number, detail => Input}))
  end;

validate_against(#mapper{}=M, Input, [#type_mapper_type{name = range, body = [From,To]}|Types], LastError) ->
  if
    From =< Input andalso Input =< To -> {ok, Input};
    true -> validate_against(M, Input, Types, or_(LastError, #{reason => out_of_range, detail => Input}))
  end;

validate_against(#mapper{}=M, Input, [#type_mapper_type{name = binary}|Types], LastError) ->
  if
    is_binary(Input) -> {ok, Input};
    is_atom(Input) -> {ok, atom_to_binary(Input,latin1)};
    true -> validate_against(M, Input, Types, or_(LastError,#{reason => non_binary, detail => Input}))
  end;

validate_against(#mapper{output=OutType}=M, Input, [#type_mapper_type{source = system, name = pid}|Types], LastError) ->
  if
    is_pid(Input) andalso OutType == record -> {ok, Input};
    is_pid(Input) andalso OutType == map -> skip;
    not is_pid(Input) -> validate_against(M, Input, Types, or_(LastError,#{reason => non_pid}))
  end;


validate_against(#mapper{}=M, Input, [#type_mapper_type{source = value, body = ImmediateValue}|Types], LastError) ->
  if
    Input == ImmediateValue orelse 
    (Input == null andalso ImmediateValue == undefined) ->
      {ok, ImmediateValue};
    is_atom(ImmediateValue) ->
      case atom_to_binary(ImmediateValue,latin1) of
        Input -> {ok, ImmediateValue};
        _ -> validate_against(M, Input, Types, LastError)
      end;
    true ->
      validate_against(M, Input, Types, LastError)
  end;

validate_against(#mapper{allow_type_convertion=Allow}=M, Input, [#type_mapper_type{name = atom}|Types], LastError) ->
  if
    is_atom(Input) -> {ok, Input};
    is_binary(Input) andalso Allow == true -> {ok, binary_to_atom(Input,latin1)};
    is_integer(Input) andalso Allow == true -> {ok, binary_to_atom(integer_to_binary(Input),latin1)};
    true -> validate_against(M, Input, Types, or_(LastError,#{reason => non_atom}))
  end;

validate_against(#mapper{}=M, Input, [#type_mapper_type{name = TName, source = user, body = undefined}|Types], LastError) ->
  case json2output(M, TName, Input) of
    {error, E} ->
      validate_against(M, Input, Types, or_(LastError, E));
    Data ->
      {ok, Data}
  end;

validate_against(#mapper{}=M, Input, [#type_mapper_type{source = user, body = Type}|Types], LastError) ->
  case validate_against(M, Input, [Type], LastError) of
    {error, E} ->
      validate_against(M, Input, Types, or_(LastError, E));
    {ok, Data} ->
      {ok, Data}
  end;

validate_against(#mapper{allow_type_convertion=Allow}=M, Input, [#type_mapper_type{name = boolean}|Types], LastError) ->
  case Input of
    true -> {ok, true};
    <<"true">> when Allow -> {ok, true};
    1 when Allow -> {ok, true};
    false -> {ok, false};
    <<"false">> when Allow -> {ok, false};
    0 when Allow -> {ok, false};
    _ -> validate_against(M, Input, Types, or_(LastError, #{reason => non_boolean_value}))
  end;


validate_against(#mapper{}=M, #{} = Input, [#type_mapper_type{name=map, body = MapFields}|Types], LastError) ->
  case fill_map_fields(M, maps:to_list(Input), MapFields, #{}) of
    {ok, Data} ->
      {ok, Data};
    {error, #{} = E} ->
      validate_against(M, Input, Types, or_(LastError, E))
  end;

validate_against(#mapper{}=M, Input, [#type_mapper_type{name=map, body = MapFields}|Types], LastError) when is_list(Input) ->
  case fill_map_fields(M, Input, MapFields, #{}) of
    {ok, Data} ->
      {ok, Data};
    {error, #{} = E} ->
      validate_against(M, Input, Types, or_(LastError, E))
  end;

validate_against(#mapper{}=M, Input, [#type_mapper_type{name=map}|Types], LastError) ->
  validate_against(M, Input, Types, or_(LastError, #{reason => non_map_value}));


validate_against(#mapper{}=M, Input, [#type_mapper_type{name=list, body = Type}|Types], LastError) when is_list(Input) ->
  Values = lists:foldr(fun
    (_I, #{} = E) ->
      E;
    (I, List) ->
      case validate_against(M, I, [Type], undefined) of
        {ok, V} -> [V|List];
        {error, #{} = E} -> E
      end
  end, [], Input),
  case Values of
    #{} -> validate_against(M, Input, Types, or_(LastError, Values));
    _ -> {ok, Values}
  end;

validate_against(#mapper{}=M, Input, [#type_mapper_type{name=list}|Types], LastError) ->
  validate_against(M, Input, Types, or_(LastError, #{reason => non_list, detail => Input}));


validate_against(#mapper{output=OutType}=M, Input, [#type_mapper_type{name=record,source=system,body=RecName}|Types], LastError) ->
  case translate_record(M, RecName, Input) of
    {error, #{} = E} ->
      validate_against(M, Input, Types, or_(LastError,E));
    Record when element(1,Record) == RecName andalso OutType == record ->
      {ok, Record};
    #{} = Output when OutType == map ->
      {ok, Output}
  end;

validate_against(#mapper{}, Input, [#type_mapper_type{name=any}|_], _) ->
  {ok, Input}.



or_(undefined, #{} = V) -> V;
or_(#{} = V, _) -> V.



prepend(Segment, #{path := Path} = Error) -> Error#{path => [Segment|Path]};
prepend(Segment, #{} = Error) -> Error#{path => [Segment]}.




fill_map_fields(#mapper{}, [], _MapFields, Acc) ->
  {ok, Acc};

fill_map_fields(#mapper{}=M, [{K,V}|Input], MapFields, Acc) ->
  case fill_map_fields2(M, K, V, MapFields, undefined, maps:size(Acc)) of
    {ok, K1, V1} ->
      Acc1 = Acc#{K1 => V1},
      fill_map_fields(M, Input, MapFields, Acc1);
    {error, #{} = E} ->
      {error, E}
  end.


record_fields(#mapper{module=Module}=M, #type_mapper_type{name=TName,source=user}) ->
  case Module:'$mapper_type'(TName) of
    #type_mapper_type{name = record}=T -> record_fields(M, T);
    _ -> undefined
  end;

record_fields(#mapper{module=Module}, #type_mapper_type{name=record,body=RName}) ->
  Module:'$mapper_record'(RName);

record_fields(#mapper{}, #type_mapper_type{}) ->
  undefined.





autofill_outer_field(OuterValue, Object, Fields, KnownType) ->
  case [Name || #type_mapper_field{name=Name, types=[#type_mapper_type{name=T}]} <- Fields, T == KnownType] of
    [] ->
      Object;
    Names ->
      lists:foldl(fun(Name, V) ->
        case maps:is_key(atom_to_binary(Name,latin1),V) of
          true -> V;
          false -> maps:merge(#{Name => OuterValue}, V)
        end
      end, Object, Names)
  end.


autofill_primary_key(Key, Value, Fields) ->
  autofill_outer_field(Key, Value, Fields, primary_key).

autofill_sort_index(Index, Value, Fields) ->
  autofill_outer_field(Index, Value, Fields, sort_index).




fill_map_fields2(#mapper{}, K, _V, [], LastError, _) ->
  {error, or_(LastError, #{reason => unknown_key, detail => K})};

fill_map_fields2(#mapper{}=M, K, V0, [{Ktype, Vtype}|MapFields], LastError, SortIndex) ->
  case validate_against(M, K, [Ktype], LastError) of
    {ok, K1} ->
      V = case V0 of
        #{} when M#mapper.fill_auto_fields == true ->
          case record_fields(M,Vtype) of
            undefined ->
              V0;
            Fields ->
              V_1 = autofill_primary_key(K1, V0, Fields),
              V_2 = autofill_sort_index(SortIndex, V_1, Fields),
              V_2
          end;
        _ ->
          V0
      end,
      case validate_against(M, V, [Vtype], LastError) of
        {ok, V1} ->
          {ok, K1, V1};
        {error, #{} = E} ->
          case Ktype of
            #type_mapper_type{source = value, body = K} ->
              {error, prepend(K,E)};
            _ ->
              fill_map_fields2(M, K, V0, MapFields, or_(LastError, prepend(K,E)), SortIndex)
          end
      end;
    {error, #{} = E} ->
      fill_map_fields2(M, K, V0, MapFields, or_(LastError, prepend(K,E)), SortIndex)
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
          R = fill_schema_dependencies_for_type(Module, #type_mapper_type{name=record,source=system,body=TName}, Acc),
          R
      end;
    #type_mapper_type{} = Type ->
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


fill_schema_dependencies_for_type(_, #type_mapper_type{name = record, body = Name}, Acc) when map_get(Name, Acc) ->
  Acc;


fill_schema_dependencies_for_type(Module, #type_mapper_type{name = record, body = RecName}, Acc) ->
  case Module:'$mapper_record'(RecName) of
    undefined ->
      Acc;
    Fields ->
      RecordSpec = fields2json_schema(Module, Fields, #{}),
      Acc1 = Acc#{RecName => RecordSpec},

      ReferencedTypes = lists:usort(lists:flatten([Types || #type_mapper_field{types = Types} <- Fields])),
      Acc2 = lists:foldl(fun(T, A) ->
        fill_schema_dependencies_for_type(Module, T, A)
      end, Acc1, ReferencedTypes),
      Acc2
  end;

fill_schema_dependencies_for_type(Module, #type_mapper_type{name=TName,source=user,body=undefined}, Acc) ->
  Acc1 = fill_schema_dependencies(Module, TName, Acc),
  Acc1;

fill_schema_dependencies_for_type(Module, #type_mapper_type{source=user,body=#type_mapper_type{} = Type}, Acc) ->
  R = fill_schema_dependencies_for_type(Module, Type, Acc),
  R;

fill_schema_dependencies_for_type(Module, #type_mapper_type{name=list,body=#type_mapper_type{} = Type}, Acc) ->
  R = fill_schema_dependencies_for_type(Module, Type, Acc),
  R;

fill_schema_dependencies_for_type(Module, #type_mapper_type{name=map,body=MapBody}, Acc) ->
  R = lists:foldl(fun({Key,Value}, A) ->
    A1 = fill_schema_dependencies_for_type(Module, Key, A),
    fill_schema_dependencies_for_type(Module, Value, A1)
  end, Acc, MapBody),
  R;

fill_schema_dependencies_for_type(_, #type_mapper_type{}, Acc) ->
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
    % #type_mapper_type{name = record, body = RecName} ->
    %   record2json_schema(Module, RecName);
    #type_mapper_type{name = record, body = RecName} ->
      #{'$ref' => <<"#/components/schemas/",(atom_to_binary(RecName,latin1))/binary>>};
    #type_mapper_type{} = T ->
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

fields2json_schema(Module, [undefined|Fields], ObjectSpec) ->
  fields2json_schema(Module, Fields, ObjectSpec);

fields2json_schema(Module, [#type_mapper_field{name=Name,default = Dfl, default_type = DflT, types=Types}|Fields], ObjectSpec) ->
  Spec1 = case Types of
    [#type_mapper_type{} = T] -> build_js_type(T);
    [_,_|_] -> #{anyOf => [build_js_type(T) || T <- Types]}
  end,
  Spec2 = case Dfl of
    undefined when DflT == undefined -> Spec1;
    DV when ?IS_TRIVIAL(DV) -> Spec1;
    DefaultValue -> Spec1#{default => DefaultValue}
  end,
  Properties1 = maps:get(properties, ObjectSpec, #{}),
  Properties2 = case Spec2 of
    undefined -> Properties1;
    _ -> Properties1#{Name => Spec2}
  end,
  ObjectSpec1 = ObjectSpec#{properties => Properties2},
  ObjectSpec2 = case Dfl of
    undefined ->
      Required = maps:get(required, ObjectSpec, []) ++ [Name],
      ObjectSpec1#{required => Required};
    _ ->
      ObjectSpec1
  end,
  fields2json_schema(Module, Fields, ObjectSpec2).






build_js_type(#type_mapper_type{name = deprecated, body = #type_mapper_type{} = Type}) ->
  #{} = Spec = build_js_type(Type),
  Spec#{deprecated => true};
build_js_type(#type_mapper_type{source = user, body = #type_mapper_type{} = Type}) ->
  #{} = Spec = build_js_type(Type),
  Spec;
build_js_type(#type_mapper_type{name = TName, source = user, body = undefined}) ->
  #{'$ref' => <<"#/components/schemas/",(atom_to_binary(TName,latin1))/binary>>};
build_js_type(#type_mapper_type{name = record, source = system, body = RName}) ->
  #{'$ref' => <<"#/components/schemas/",(atom_to_binary(RName,latin1))/binary>>};
build_js_type(#type_mapper_type{name = non_neg_integer}) -> #{type => number};
build_js_type(#type_mapper_type{name = integer}) -> #{type => number};
build_js_type(#type_mapper_type{name = number}) -> #{type => number};
build_js_type(#type_mapper_type{name = float}) -> #{type => number};
build_js_type(#type_mapper_type{name = range, body = [From,To]}) -> #{type => number, minimum => From, maximum => To};
build_js_type(#type_mapper_type{name = binary}) -> #{type => string};
build_js_type(#type_mapper_type{name = boolean}) -> #{type => boolean};
build_js_type(#type_mapper_type{name = list, body = B}) -> #{type => array, items => build_js_type(B)};
build_js_type(#type_mapper_type{name = atom,source=value,body=V}) -> #{type => string, enum => [V]};
build_js_type(#type_mapper_type{name = atom}) -> #{type => string};
build_js_type(#type_mapper_type{name = pid}) -> undefined;
build_js_type(#type_mapper_type{name = map, body = MapBody}) ->
  build_js_map(MapBody, #{
    type => object
  });
build_js_type(#type_mapper_type{name = any}) -> #{type => object}.



build_js_map([], Spec) ->
  Spec;
build_js_map([{#type_mapper_type{source=value,body=Key},Value}|MapBody], #{} = Spec) ->
  Properties = (maps:get(properties, Spec, #{}))#{Key => build_js_type(Value)},
  build_js_map(MapBody, Spec#{properties => Properties});

build_js_map([{#type_mapper_type{},Value}|MapBody], #{} = Spec) ->
  Properties = (maps:get(patternProperties, Spec, #{}))#{<<".*">> => build_js_type(Value)},
  build_js_map(MapBody, Spec#{patternProperties => Properties}).







