-module(type_mapper).
-include("../include/type_mapper.hrl").
-export([src/1, parse_transform/2]).

-export([record/3, record/4, map/3, map/4, json_schema/2]).
-export([process/2]).


-define(IS_TRIVIAL(X), (X == #{} orelse X == undefined orelse X == [])).

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
  R = process(Options#{output => map, module => Module, type_name => TName}, Input),
  R.


record(Module, TName, Input) ->
  record(Module, TName, Input, #{}).

record(Module, TName, Input, #{} = Options) ->
  R = process(Options#{output => record, module => Module, type_name => TName}, Input),
  R.





process(#{} = Opts, Input) ->
  Convertion = maps:with([allow_type_convertion], Opts),
  Validation = maps:merge(Opts, #{
    check_numbers => true
  }),
  Callbacks = case maps:get(query_support, Opts, false) of
    true -> 
      [
        {tm_query_support,Opts},
        {tm_json,Opts}
      ];
    false ->
      [
        % {tm_logger,log},
        {tm_converter,Convertion},
        {tm_validator,Validation},
        {tm_json,Opts}
      ]
  end,
  case tm_visitor:visit(Opts#{callbacks => Callbacks}, Input) of
    {ok, V} -> V;
    {error, E} -> {error, E}
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
      build_js_any_types(Types)
  end.

build_js_any_types(Types) ->
  Types1 = [build_js_type(T) || T <- Types],
  Types2 = try_to_collapse(Types1),
  case Types2 of
    [T] -> T;
    _ -> #{anyOf => Types2}
  end.

try_to_collapse([#{type := string, enum := Enum} = T|Types]) ->
  {Enums, Others} = lists:partition(fun(#{} = T1) ->
    case T1 of
      #{type := string, enum := _} -> true;
      #{} -> false
    end
  end, Types),

  Values = lists:flatmap(fun(#{type := string, enum := E}) ->
    E
  end, Enums),
  [T#{enum => Enum ++ Values}|try_to_collapse(Others)];

try_to_collapse([T|Types]) ->
  [T|try_to_collapse(Types -- [T])];

try_to_collapse([]) ->
  [].




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
    [_,_|_] -> build_js_any_types(Types)
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
  ObjectSpec2 = case DflT of
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
build_js_type(#type_mapper_type{name = pos_integer}) -> #{type => number};
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
  % Properties = (maps:get(patternProperties, Spec, #{}))#{<<".*">> => build_js_type(Value)},
  % build_js_map(MapBody, Spec#{patternProperties => Properties}).
  build_js_map(MapBody, Spec#{additionalProperties => build_js_type(Value)}).







