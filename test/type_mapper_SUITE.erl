-module(type_mapper_SUITE).
-compile(nowarn_export_all).
-compile(nowarn_unused_record).
-compile(nowarn_unused_type).
-compile(export_all).

-compile({parse_transform,type_mapper}).

all() ->
  [
    equal_map_to_record,
    non_neg_integer2rec,
    non_neg_integer2json,
    non_neg_integer2map,
    non_neg_integer_schema,

    non_neg_integer_with_undefined,
    integer_with_default,
    integer_with_default_schema,
    binary,
    outer_rec2rec,
    outer_rec2json,
    atom_value,
    atom_value_autoconvert,
    atom_value_schema,
    atom_binary_value_schema,
    atom_type,
    atom_type_schema,
    boolean,
    boolean_schema,
    wrapped_type,
    user_union,
    user_union_schema,
    map_type,
    map_type2map,
    map_type_schema,


    map_with_values,
    map_with_values_schema,
    typed_map,
    list_type,
    list_type_with_undefined,
    list_type_schema,
    any_value,
    ranged_integer,

    parameterised_type,
    parameterised_type_schema,

    non_setting_default_undefined_to_map,

    number,
    number_schema,
    float_schema,

    any,
    any_schema,

    untyped_schema,


    referenced_types_schema1,
    referenced_types_schema2,
    referenced_types_schema3,
    referenced_types_schema4,


    validate_nontyped_record,
    nontyped_record_schema,

    erlang_pid_type,
    erlang_pid_schema,


    default_value,
    % default_undefined_to_map,
    default_value_not_undefined,
    default_blank_value_not_in_map,
    default_value_with_forced_undefined,
    skip_map_defaults,

    atom_or_record,

    allow_miss_mandatory,

    query_params_validation,
    custom_types_validation
  ].


-record(non_neg_integer, {
  key :: non_neg_integer()
}).

-type t_non_neg_int() :: #non_neg_integer{}.

non_neg_integer2rec(_) ->
  #non_neg_integer{key = 15} = type_mapper:record(?MODULE, t_non_neg_int, #{<<"key">> => 15}),
  #non_neg_integer{key = 15} = type_mapper:record(?MODULE, t_non_neg_int, #{key => 15}),
  {error, #{path := [t_non_neg_int,key], reason := lacks_mandatory}} = type_mapper:record(?MODULE, t_non_neg_int, #{}),
  {error, #{path := [t_non_neg_int,key], reason := negative_integer}} = type_mapper:record(?MODULE, t_non_neg_int, #{key => -15}),
  {error, #{path := [t_non_neg_int,key], reason := non_integer}} = type_mapper:record(?MODULE, t_non_neg_int, #{key => <<"15">>}),
  #non_neg_integer{key = 15} = type_mapper:record(?MODULE, t_non_neg_int, #{key => <<"15">>}, #{allow_type_convertion => true}),
  ok.

non_neg_integer2json(_) ->
  #{key := 15} = type_mapper:map(?MODULE, t_non_neg_int, #non_neg_integer{key = 15}),
  #{key := 15} = type_mapper:map(?MODULE, t_non_neg_int, #{key => 15}),
  {error, #{reason := lacks_mandatory}} = type_mapper:map(?MODULE, t_non_neg_int, #{a => 5}),
  ok.



non_neg_integer2map(_) ->
  #{key := 15} = type_mapper:map(?MODULE, t_non_neg_int, #{<<"key">> => 15}),
  #{key := 15} = type_mapper:map(?MODULE, t_non_neg_int, #{key => 15}),
  {error, #{path := [t_non_neg_int,key], reason := lacks_mandatory}} = type_mapper:map(?MODULE, t_non_neg_int, #{}),
  {error, #{path := [t_non_neg_int,key], reason := negative_integer}} = type_mapper:map(?MODULE, t_non_neg_int, #{key => -15}),
  {error, #{path := [t_non_neg_int,key], reason := non_integer}} = type_mapper:map(?MODULE, t_non_neg_int, #{key => <<"15">>}),
  ok.



non_neg_integer_schema(_) ->
  #{components := #{schemas := #{non_neg_integer := #{properties := #{ key := #{type := number}},
  required := [key]}}},
    '$ref' := <<"#/components/schemas/non_neg_integer">>} = type_mapper:json_schema(?MODULE, t_non_neg_int),
  ok.








-record(non_neg_integer_with_undefined, {
  key :: non_neg_integer() | undefined
}).

-type non_neg_integer_with_undefined() :: #non_neg_integer_with_undefined{}.

non_neg_integer_with_undefined(_) ->
  #non_neg_integer_with_undefined{key = 15} = type_mapper:record(?MODULE, non_neg_integer_with_undefined, #{<<"key">> => 15}),
  #non_neg_integer_with_undefined{key = 15} = type_mapper:record(?MODULE, non_neg_integer_with_undefined, #{key => 15}),
  #non_neg_integer_with_undefined{key = undefined} = type_mapper:record(?MODULE, non_neg_integer_with_undefined, #{key => undefined}),
  #non_neg_integer_with_undefined{key = undefined} = type_mapper:record(?MODULE, non_neg_integer_with_undefined, #{key => null}),
  % Вопрос: если у нас допустим undefined среди типов, то считаем ли мы его
  % дефолтным значением, которое можно не указывать? Пока что решаем, что нет, нельзя
  % #non_neg_integer_with_undefined{key = undefined} = type_mapper:record(?MODULE, non_neg_integer_with_undefined, #{}),
  {error, #{path := [non_neg_integer_with_undefined,key], reason := lacks_mandatory}} = type_mapper:record(?MODULE, non_neg_integer_with_undefined, #{}),
  {error, #{path := [non_neg_integer_with_undefined,key], reason := negative_integer}} = type_mapper:record(?MODULE, non_neg_integer_with_undefined, #{key => -15}),
  {error, #{path := [non_neg_integer_with_undefined,key], reason := non_integer}} = type_mapper:record(?MODULE, non_neg_integer_with_undefined, #{key => <<"15">>}),
  ok.


-record(integer_with_default, {
  key = 5 :: integer()
}).

-type integer_with_default() :: #integer_with_default{}.

integer_with_default(_) ->
  #integer_with_default{key = 15} = type_mapper:record(?MODULE, integer_with_default, #{<<"key">> => 15}),
  #integer_with_default{key = -15} = type_mapper:record(?MODULE, integer_with_default, #{key => -15}),
  #integer_with_default{key = 5} = type_mapper:record(?MODULE, integer_with_default, #{}),
  {error, #{path := [integer_with_default,key], reason := non_integer}} = type_mapper:record(?MODULE, integer_with_default, #{key => <<"15">>}),
  #integer_with_default{key = 15} = type_mapper:record(?MODULE, integer_with_default, #{<<"key">> => <<"15">>}, #{allow_type_convertion => true}),

  #{key := 5} = type_mapper:map(?MODULE, integer_with_default, #{}, #{skip_map_defaults => false}),
  #{key := 5} = type_mapper:map(?MODULE, integer_with_default, #{}, #{skip_map_defaults => trivial}),
  #{key := 5} = type_mapper:map(?MODULE, integer_with_default, #{}, #{}),
  ok.


integer_with_default_schema(_) ->
 #{components := #{schemas := #{integer_with_default := #{
    properties := #{
      key := #{type := number, default := 5}
    }
  }}},
  '$ref' := <<"#/components/schemas/integer_with_default">>} = type_mapper:json_schema(?MODULE, integer_with_default).





-record(nested_rec, {
  key :: integer()
}).

-type nested_rec() :: #nested_rec{}.

-record(outer_rec, {
  nested :: nested_rec()
}).

-type outer_rec() :: #outer_rec{}.

outer_rec2rec(_) ->
  #outer_rec{nested = #nested_rec{key = 10}} = type_mapper:record(?MODULE, outer_rec, #{nested => #{key => 10}}),
  #outer_rec{nested = #nested_rec{key = 10}} = type_mapper:record(?MODULE, outer_rec, #{<<"nested">> => #{key => 10}}),
  #outer_rec{nested = #nested_rec{key = 10}} = type_mapper:record(?MODULE, outer_rec, #{<<"nested">> => #{<<"key">> => 10}}),

  #outer_rec{nested = #nested_rec{key = 10}} = type_mapper:record(?MODULE, outer_rec, #{<<"nested">> => #{<<"key">> => <<"10">>}},
    #{allow_type_convertion => true}),
  ok.


outer_rec2json(_) ->
  #{nested := #{key := 10}} = type_mapper:map(?MODULE, outer_rec, #outer_rec{nested = #nested_rec{key = 10}}),
  ok.




-type atom_key2_type() :: true|false|other.

-record(atom_value, {
  key :: a | b | c | non_neg_integer(),
  key2 = true :: atom_key2_type()
}).


-type atom_value() :: #atom_value{}.

atom_value(_) ->
  #atom_value{key = a} = type_mapper:record(?MODULE, atom_value, #{key => <<"a">>}),
  #atom_value{key = 5} = type_mapper:record(?MODULE, atom_value, #{key => 5}),
  ok.



atom_value_autoconvert(_) ->
  #atom_value{key2 = true} = type_mapper:record(?MODULE, atom_value, #{key => <<"a">>, key2 => <<"true">>},
    #{allow_type_convertion => true}),
  #atom_value{key2 = other} = type_mapper:record(?MODULE, atom_value, #{key => <<"a">>, key2 => <<"other">>},
    #{allow_type_convertion => true}),
  % {error, #{}} = type_mapper:record(?MODULE, atom_value, #{key => <<"a">>, key2 => <<"other">>}),
  ok.


atom_value_schema(_) ->
  #{components := #{schemas := #{atom_value := #{properties := #{
    key := #{
      anyOf := [
        #{type := string, enum := [a,b,c]},
        #{type := number}
      ]
    }
  }}}},
  '$ref' := <<"#/components/schemas/atom_value">>} = type_mapper:json_schema(?MODULE, atom_value),
  ok.



-type atom_binary_value() :: atom() | binary().

atom_binary_value_schema(_) ->
  #{type := string} = type_mapper:json_schema(?MODULE, atom_binary_value),
  ok.




-type atom_type() :: atom().


atom_type(_) ->
  a = type_mapper:record(?MODULE, atom_type, a),
  a = type_mapper:record(?MODULE, atom_type, <<"a">>, #{allow_type_convertion => true}),
  {error, #{reason := non_atom, path := [atom_type]}} = type_mapper:record(?MODULE, atom_type, <<"a">>),
  {error, #{reason := non_atom, path := [atom_type]}} = type_mapper:record(?MODULE, atom_type, #{}),
  ok.

atom_type_schema(_) ->
  #{type := string} = type_mapper:json_schema(?MODULE, atom_type).




-type map_with_values() :: #{
  key1 => integer(),
  atom() => binary()
}.

map_with_values(_) ->
  #{key1 := 5, key2 := <<"6">>} = type_mapper:record(?MODULE, map_with_values, #{<<"key1">> => 5, key2 => <<"6">>}),
  % Question: should we autoconvert unknown binary to atom??
  % #{key1 := 5, key2 := <<"6">>} = type_mapper:record(?MODULE, map_with_values, #{<<"key1">> => 5, <<"key2">> => <<"6">>}),
  {error, #{reason := unmatched_type, path := [map_with_values,<<"key2">>]}} = 
    type_mapper:record(?MODULE, map_with_values, #{<<"key1">> => 5, <<"key2">> => <<"6">>}),
  #{key1 := 5, key2 := <<"6">>} = type_mapper:record(?MODULE, map_with_values, #{key1 => 5, key2 => <<"6">>}),
  {error, #{reason := non_integer, path := [map_with_values, <<"key1">>]}} = 
    type_mapper:record(?MODULE, map_with_values, #{<<"key1">> => <<"5">>}),
  ok.


map_with_values_schema(_) ->
  #{
    type := object,
    properties := #{
      key1 := #{type := number}
    },
    additionalProperties := #{type := string}
  } = type_mapper:json_schema(?MODULE, map_with_values).









-record(boolean, {
  key = undefined :: boolean()
}).

-type t_boolean() :: #boolean{}.

boolean(_) ->
  #boolean{key = true} = type_mapper:record(?MODULE, t_boolean, #{key => <<"true">>}, #{allow_type_convertion => true}),
  {error, #{}} = type_mapper:record(?MODULE, t_boolean, #{key => <<"true">>}),
  #boolean{key = true} = type_mapper:record(?MODULE, t_boolean, #{key => true}),
  #boolean{key = false} = type_mapper:record(?MODULE, t_boolean, #{key => <<"false">>}, #{allow_type_convertion => true}),
  {error, #{}} = type_mapper:record(?MODULE, t_boolean, #{key => <<"false">>}),
  #boolean{key = false} = type_mapper:record(?MODULE, t_boolean, #{key => false}),
  ok.


boolean_schema(_) ->
  #{components := #{schemas := #{boolean := Props}},
  '$ref' := <<"#/components/schemas/boolean">>} = type_mapper:json_schema(?MODULE, t_boolean),
  #{properties := #{
    key := #{type := boolean}
  },
  type := object} = Props,
  [properties,type] = maps:keys(Props),
  ok.




-type my_own_boolean() :: boolean().

-type wrap(X) :: X.

-record(wrapped_type, {
  key :: my_own_boolean()
}).

-type wrapped_type() :: wrap(#wrapped_type{}).

wrapped_type(_) ->
  #wrapped_type{key = true} = type_mapper:record(?MODULE, wrapped_type, #{key => true}),
  ok.



-type user_union() :: a|b|c.

-record(user_unioned, {
  key :: user_union()
}).

-type user_unioned() :: #user_unioned{}.

user_union(_) ->
  #user_unioned{key = a} = type_mapper:record(?MODULE, user_unioned, #{<<"key">> => <<"a">>}),
  ok.


user_union_schema(_) ->
  #{components :=
    #{schemas := #{
      user_union := #{
      
      },
      user_unioned := #{
        type := object,
        properties := #{
          key := #{
            '$ref' := <<"#/components/schemas/user_union">>
          }
        }
      }
    }},
    '$ref' := <<"#/components/schemas/user_unioned">>
  } = type_mapper:json_schema(?MODULE, user_unioned).


-record(item, {
  title :: binary()
}).

-type item() :: #item{}.

-record(map_container, {
  items = #{} :: #{binary() => item()},
  meta = undefined :: #{atom() => binary()}
}).

-type map_container() :: #map_container{}.


map_type(_) ->
  #map_container{items = #{<<"a">> := #item{title = <<"b">>} }} =
    type_mapper:record(?MODULE, map_container, #{<<"items">> => #{<<"a">> => #{<<"title">> => <<"b">>}}}),
  ok.


map_type2map(_) ->
  #{items := #{<<"a">> := #{title := <<"b">>} }} =
    type_mapper:map(?MODULE, map_container, #{<<"items">> => #{<<"a">> => #{<<"title">> => <<"b">>}}}),

  {error, #{reason := non_map_input}} = type_mapper:map(?MODULE, map_container, #{<<"items">> => #{<<"a">> => null}}),

  #{items := #{<<"a">> := undefined}} = type_mapper:map(?MODULE, map_container, #{<<"items">> => #{<<"a">> => null}},
    #{allow_forced_undefined => true}),

  #{meta := #{a := undefined}} = type_mapper:map(?MODULE, map_container, #{<<"meta">> => #{<<"a">> => undefined}},
    #{allow_forced_undefined => true, allow_type_convertion => true}),
  #{meta := #{a := undefined}} = type_mapper:map(?MODULE, map_container, #{<<"meta">> => #{<<"a">> => null}},
    #{allow_forced_undefined => true, allow_type_convertion => true}),
  ok.


map_type_schema(_) ->
  #{
    components := #{
      schemas := #{
        item := #{},
        map_container := #{
          type := object,
          properties := #{
            items := #{
              type := object,
              additionalProperties := #{'$ref' := <<"#/components/schemas/item">>}
            }
          }        
        }
      }
    },
    '$ref' := <<"#/components/schemas/map_container">>
  } = type_mapper:json_schema(?MODULE, map_container).









-type typed_map() :: #{a => 1, b => 2, atom() => binary()}.

typed_map(_) ->
  #{a := 1, b := 2} = type_mapper:record(?MODULE, typed_map, #{a => 1, b => 2}),
  #{a := 1} = type_mapper:record(?MODULE, typed_map, #{a => 1}),
  #{a := 1, b := 2, c := <<"a">>} = type_mapper:record(?MODULE, typed_map, #{a => 1, b => 2, c => <<"a">>}),
  {error, #{reason := unmatched_type}} = type_mapper:record(?MODULE, typed_map, #{a => 1, b => 2, c => 3}),
  #{a := 1, b := 2} = type_mapper:record(?MODULE, typed_map, #{<<"a">> => 1, b => 2}),
  {error, #{path := [typed_map,a],reason := non_integer}} = type_mapper:record(?MODULE, typed_map, #{a => <<"1">>}),
  ok.






-record(binary_rec, {
  key :: binary()
}).

-type binary_rec() :: #binary_rec{}.

binary(_) ->
  #binary_rec{key = <<"value">>} = type_mapper:record(?MODULE, binary_rec, #{key => <<"value">>}),
  #binary_rec{key = <<"value">>} = type_mapper:record(?MODULE, binary_rec, #{key => value}),
  ok.



-record(list_container, {
  items = [] :: [item()]
}).

-type list_container() :: #list_container{}.

list_type(_) ->
  #list_container{items = [#item{title = <<"a">>}]} = 
    type_mapper:record(?MODULE, list_container, #{items => [#{title => <<"a">>}]}),

  {error, #{reason := non_list, path := [list_container, items]}} =
    type_mapper:record(?MODULE, list_container, #{items => #{title => <<"a">>}}),

  ok.







-record(list_type_with_undefined, {
  items = undefined :: [number()]
}).


list_type_with_undefined(_) ->
  #list_type_with_undefined{items = undefined} =
    type_mapper:record(?MODULE, list_type_with_undefined, #{items => undefined}),

  % FIXME: need to decide which one is correct
  % {error, #{detail := null}} = type_mapper:record(?MODULE, list_type_with_undefined, #{items => null}),
  % #list_type_with_undefined{items = undefined} = type_mapper:record(?MODULE, list_type_with_undefined, #{items => null}),
  #list_type_with_undefined{items = undefined} =
    type_mapper:record(?MODULE, list_type_with_undefined, #{items => null}, #{allow_type_convertion => true}),
  ok.


list_type_schema(_) ->
  #{
  components := #{
    schemas := #{
      item := #{
        type := object,
        properties := #{
          title := #{
            type := string
          }
        }
      },
      list_container := #{
        type := object,
        properties := #{
          items := #{
            type := array,
            items := #{
              '$ref' := <<"#/components/schemas/item">>
            }
          }
        }
      }
    }
  },
  '$ref' := <<"#/components/schemas/list_container">>
} = type_mapper:json_schema(?MODULE, list_container).



-record(any_rec, {
  item :: any()
}).

-type any_rec() :: #any_rec{}.

any_value(_) ->
  #any_rec{item = 5} = type_mapper:record(?MODULE, any_rec, #{item => 5}),
  #any_rec{item = <<"5">>} = type_mapper:record(?MODULE, any_rec, #{item => <<"5">>}),
  ok.



-type listen_port() :: 1..65000.

-type listen_spec() :: listen_port() | binary().

ranged_integer(_) ->
  5 = type_mapper:record(?MODULE, listen_spec, 5),
  {error, #{reason := out_of_range}} = type_mapper:record(?MODULE, listen_spec, 0),
  {error, #{reason := out_of_range}} = type_mapper:record(?MODULE, listen_spec, 100000),
  {error, #{reason := out_of_range}} = type_mapper:record(?MODULE, listen_port, <<"5">>),
  5 = type_mapper:record(?MODULE, listen_port, <<"5">>, #{allow_type_convertion => true}),
  <<"spec">> = type_mapper:record(?MODULE, listen_spec, <<"spec">>),
  ok.





-type deprecated(X) :: X.
-type runtime(X) :: X.

-type deprecated_integer() :: deprecated(integer()).
-type runtime_integer() :: runtime(integer()).
-record(inner_record, {
  key = undefined :: integer()
}).
-type runtime_record() :: runtime(#inner_record{}).

parameterised_type(_) ->
  5 = type_mapper:record(?MODULE, deprecated_integer, 5),
  {error, #{reason := non_integer}} = type_mapper:record(?MODULE, deprecated_integer, <<"5">>),

  5 = type_mapper:record(?MODULE, runtime_integer, 5),
  {error, #{reason := non_integer}} = type_mapper:record(?MODULE, runtime_integer, <<"5">>),

  #inner_record{key = 5} = type_mapper:record(?MODULE, runtime_record, #{key =>5}),

  ok.



parameterised_type_schema(_) ->
  #{
    type := number,
    deprecated := true
  } = type_mapper:json_schema(?MODULE, deprecated_integer),
  #{
    type := number
  } = type_mapper:json_schema(?MODULE, runtime_integer),

  % This case is not very clean, because it requires joining namespace of types and records.
  % Do we really want it?
  #{
    components := #{
      schemas := #{
        inner_record := #{
          properties := #{
            key := #{type := number}
          }
        }
      }
    },
    '$ref' := <<"#/components/schemas/inner_record">>
  } = type_mapper:json_schema(?MODULE, runtime_record),
  ok.  




-type any_type() :: any().

any(_) ->
  5 = type_mapper:record(?MODULE, any_type, 5),
  <<"5">> = type_mapper:record(?MODULE, any_type, <<"5">>),
  ok.


any_schema(_) ->
  #{type := object} = type_mapper:json_schema(?MODULE, any_type).



-record(rec_with_untyped, {
  key1 = undefined :: integer(),
  key2
}).


untyped_schema(_) ->
  #{
    type := object,
    properties := #{
      key1 := #{type := number}
    } = Props
  } = type_mapper:json_schema(?MODULE, rec_with_untyped),
  [key1] = maps:keys(Props),
  ok.




-record(map_with_undefined, {
  key = undefined :: integer()
}).

-type map_with_undefined() :: #map_with_undefined{}.

non_setting_default_undefined_to_map(_) ->
  #{} = Output = type_mapper:map(?MODULE, map_with_undefined, #{}),
  false = maps:is_key(key, Output),
  ok.


-type number_type() :: number().

number(_) ->
  5 = type_mapper:record(?MODULE, number_type, 5),
  % TODO: Do we want it ?
  % 5 = type_mapper:record(?MODULE, number_type, <<"5">>),
  5.5 = type_mapper:record(?MODULE, number_type, 5.5),
  % 5.5 = type_mapper:record(?MODULE, number_type, <<"5.5">>),
  {error, #{reason := non_number}} = type_mapper:record(?MODULE, number_type, aaa),
  ok.



number_schema(_) ->
  #{type := number} = type_mapper:json_schema(?MODULE, number_type).


-type float_type() :: float().

float_schema(_) ->
  #{type := number} = type_mapper:json_schema(?MODULE, float_type).





-record(rts_record1, {
  field1 = undefined :: integer()
}).

-type rec2_integer() :: integer().

-record(rts_record2, {
  field2 = undefined :: #rts_record1{},
  field2_2 = undefined :: rec2_integer()
}).

-record(rts_record3, {
  field3 = undefined :: #rts_record2{}
}).

-type rts_record3() :: #rts_record3{}.

-record(rts_record4, {
  field4 = undefined :: rts_record3()
}).

-type rts_record4() :: #rts_record4{}.


referenced_types_schema1(_) ->
  #{
    type := object,
    properties := #{
      field1 := #{type := number}
    }
  } = type_mapper:json_schema(?MODULE, rts_record1).

referenced_types_schema2(_) ->
  #{
    components := #{
      schemas := #{
        rts_record1 := #{
          type := object,
          properties := #{
            field1 := #{type := number}
          }          
        },
        rec2_integer := #{
          type := number
        }
      }
    },
    type := object,
    properties := #{
      field2 := #{
        '$ref' := <<"#/components/schemas/rts_record1">>
      },
      field2_2 := #{
        '$ref' := <<"#/components/schemas/rec2_integer">>      
      }
    }
  } = type_mapper:json_schema(?MODULE, rts_record2).

referenced_types_schema3(_) ->
  #{
    components := #{
      schemas := #{
        rts_record1 := #{},
        rts_record2 := #{},
        rts_record3 := #{}
      }
    },
    '$ref' := <<"#/components/schemas/rts_record3">>
  } = type_mapper:json_schema(?MODULE, rts_record3).



referenced_types_schema4(_) ->
  #{
    components := #{
      schemas := #{
        rts_record1 := #{},
        rts_record2 := #{},
        rts_record3 := #{},
        rts_record4 := #{
          properties := #{
            field4 := #{'$ref' := <<"#/components/schemas/rts_record3">>}
          }
        }
      }
    },
    '$ref' := <<"#/components/schemas/rts_record4">>
  } = type_mapper:json_schema(?MODULE, rts_record4).





-record(nontyped_record, {
  key = undefined :: non_neg_integer()
}).

validate_nontyped_record(_) ->
  #nontyped_record{key = 5} = type_mapper:record(?MODULE, nontyped_record, #{<<"key">> => 5}),
  ok.


nontyped_record_schema(_) ->
  #{
    components := #{
      schemas := #{
        nontyped_record := #{
          properties := #{
            key := #{}
          }
        }
      }
    },
    properties := #{key := #{type := number}},
    type := object
  } = type_mapper:json_schema(?MODULE, nontyped_record).



-record(rec_with_pid, {
  key1 = undefined :: pid(),
  key2 = undefined :: non_neg_integer()
}).

erlang_pid_type(_) ->
  Self = self(),
  #rec_with_pid{key1 = Self, key2 = 5} = type_mapper:record(?MODULE, rec_with_pid, #{key1 => Self, key2 => 5}),
  [{key2,5}] = maps:to_list(type_mapper:map(?MODULE, rec_with_pid, #{key1 => Self, key2 => 5})),
  ok.


erlang_pid_schema(_) ->
  #{properties := Properties} = type_mapper:json_schema(?MODULE, rec_with_pid),
  [{key2, #{type := number}}] = maps:to_list(Properties),
  ok.





-record(rec_with_default, {
  key = undefined :: number()
}).


default_value(_) ->
  #rec_with_default{key = undefined} = type_mapper:record(?MODULE, rec_with_default, #{key => undefined}).



-record(default_undefined_to_map, {
  key = undefined :: any()
}).

default_undefined_to_map(_) ->
  #{} = Map = type_mapper:map(?MODULE, default_undefined_to_map, #default_undefined_to_map{}, #{skip_map_defaults => true}),
  #{} == Map orelse error([useless_keys_in_map, Map]),
  [] = maps:keys(Map).


-record(rec_with_default_non_undefined, {
  key = true :: boolean(),
  key_false = false :: boolean(),
  items_undef = undefined :: [number()],
  items_list = [] :: [number()],
  subrec = #rec_with_default{} :: #rec_with_default{},
  submap = #{} :: #{}
}).

default_value_not_undefined(_) ->
  #rec_with_default_non_undefined{key = true, submap = #{},
    subrec = #rec_with_default{key = undefined}} = 
    type_mapper:record(?MODULE, rec_with_default_non_undefined, #{key => true}),
  #{key := true, submap := #{}, subrec := #{}} =
    type_mapper:map(?MODULE, rec_with_default_non_undefined, #{key => true}, #{skip_map_defaults => false}),
  ok.




default_blank_value_not_in_map(_) ->
  Res = type_mapper:map(?MODULE, rec_with_default_non_undefined, #{}, #{skip_map_defaults => true}),
  [] == maps:keys(Res) orelse begin
    ct:pal("was waiting for empty, map, got: ~p", [Res]),
    error([not_empty_map,Res])
  end,
  ok.


default_value_with_forced_undefined(_) ->
  #{items_list := undefined} = type_mapper:map(?MODULE, rec_with_default_non_undefined,
    #{<<"items_list">> => null}, #{allow_forced_undefined => true}),

  #{items_list := undefined} = type_mapper:map(?MODULE, rec_with_default_non_undefined,
    #{<<"items_list">> => undefined}, #{allow_forced_undefined => true, skip_map_defaults => true}),
  ok.


skip_map_defaults(_) ->
  #{items_undef := undefined} = type_mapper:map(?MODULE, rec_with_default_non_undefined, 
    #{<<"items_undef">> => undefined}, #{skip_map_defaults => trivial}),

  #{key_false := false} = type_mapper:map(?MODULE, rec_with_default_non_undefined, 
    #{<<"key_false">> => false}, #{skip_map_defaults => true}),
  ok.



-record(atom_or_record_rec, {
  key = undefined :: non_neg_integer()
}).

-type atom_or_record() :: #atom_or_record_rec{} | atom.

atom_or_record(_) ->
  atom = type_mapper:record(?MODULE, atom_or_record, atom).



equal_map_to_record(_) ->
  Records = ?MODULE:'$mapper_records'(),
  lists:map(fun(RName) ->
    M1 = type_mapper:map(?MODULE, RName, #{}),
    R2 = type_mapper:record(?MODULE, RName, M1),
    M3 = type_mapper:map(?MODULE, RName, R2),
    R4 = type_mapper:record(?MODULE, RName, M3),
    M5 = type_mapper:map(?MODULE, RName, R4),
    R6 = type_mapper:record(?MODULE, RName, M5),
    R4 == R6 orelse begin
      ct:pal("R4: ~p\nR6: ~p\n", [R4, R6]),
      error([r4_r6,RName])
    end,
    M5 == M3 orelse begin
      ct:pal("M5: ~p\nM3: ~p\n", [M5, M3]),
      error([m5_m3,RName])
    end
  end, Records),
  ok.




-record(mandatory_field, {
  name :: binary()
}).


allow_miss_mandatory(_) ->
  {error, #{reason := non_map_input}} = type_mapper:map(?MODULE, mandatory_field, {error, #{}}),
  Map1 = type_mapper:map(?MODULE, mandatory_field, #{}, #{allow_miss_mandatory => true}),
  Map1 = #{},
  Map2 = #{name => <<"name">>},
  Map2 = type_mapper:map(?MODULE, mandatory_field, Map2),
  ok.








-type hexcolor() :: binary().


custom_types_validation(_) ->
  <<"badhexcolor">> = type_mapper:record(?MODULE, hexcolor, <<"badhexcolor">>),
  ValidatorConverter = fun
    (<<"#",RGB:6/binary>>) -> {ok, RGB};
    (<<RGB:6/binary>>) -> {ok, RGB};
    (_Input) -> {error, #{reason => invalid_input}}
  end,

  Validator = fun
    (<<RGB:6/binary>>) -> {ok, RGB};
    (_Input) -> {error, #{reason => invalid_input}}
  end,
  {error, #{}} = type_mapper:record(?MODULE, hexcolor, <<"badhexcolor">>, #{validators => #{hexcolor => Validator}}),
  <<"000000">> = type_mapper:record(?MODULE, hexcolor, <<"#000000">>, 
      #{validators => #{hexcolor => ValidatorConverter}, allow_type_convertion => true}),
  {error, _} = type_mapper:record(?MODULE, hexcolor, <<"#000000">>, #{validators => #{hexcolor => Validator}}),
  <<"000000">> = type_mapper:record(?MODULE, hexcolor, <<"000000">>, #{validators => #{hexcolor => Validator}}),
  ok.




-type queried_record_type() :: a | b | c.

-record(queried_record, {
  name = undefined :: binary(),
  type = undefined :: a | b | c,
  type2 = undefined :: queried_record_type(),
  bytes = undefined :: non_neg_integer()
}).

query_params_validation(_) ->
  #{
    name := [<<"a">>,<<"b">>],
    bytes := #{'$gt' := 1500},
    type := [a],
    type2 := [b,c]
  } = type_mapper:map(?MODULE, queried_record,
    #{<<"name">> => [<<"a">>,<<"b">>], <<"type">> => [<<"a">>],
    <<"type2">> => [<<"b">>,<<"c">>], <<"bytes">> => #{'$gt' => <<"1500">>}},
    #{allow_type_convertion => true, skip_map_defaults => true, query_support => true}),
  ok.


