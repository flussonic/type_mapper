# type_mapper
Erlang type mapping tool that allows to convert json to structured data according to type specs.


It can:

* translate JSON to records according to typespecs
* translate JSON to valid typed maps according to typespecs
* translate records to valid typed maps
* validate input and return errors if they happen just like ninenines/sheriff
* give JSON schema output suitable for Swagger


It will:

* read function specifications to export API specs
* export Swagger specs
* be compatible with Protobuf and gRPC



# Convert JSON to records

You can use it to convert weakly structured and typed json input into strict type-checked erlang records or maps.



This test shows what was all this for.

```
-module(my_api).
-compile(nowarn_unused_record).
-compile(nowarn_unused_type).
-compile({parse_transform,type_mapper}).

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

  Text = <<"{\"nested\":{\"key\":\"10\"}}">>,
  #outer_rec{nested = #nested_rec{key = 10}} = type_mapper:record(?MODULE, outer_rec, jsx:decode(Text,[return_maps])),
  ok.


outer_rec2json(_) ->
  #{nested := #{key := 10}} = type_mapper:map(?MODULE, outer_rec, #outer_rec{nested = #nested_rec{key = 10}}),
  ok.

```


# Validate user input

Second important part is validation. Here is an example:

```
  {error, #{path := [t_non_neg_int,key], reason := non_integer}} = type_mapper:map(?MODULE, t_non_neg_int, #{key => <<"15">>}),
```


