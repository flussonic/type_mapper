-module(tm_converter).
-include("../include/type_mapper.hrl").

-export([tm_handle_scalar/3, tm_handle_record_field/4, tm_handle_close_record/4]).


tm_handle_scalar(#{allow_type_convertion := true}, <<_/binary>> = Input, #type_mapper_type{name=IntType}) when 
  (IntType == non_neg_integer orelse IntType == integer orelse IntType == number orelse IntType == range orelse IntType == pos_integer) ->
  case string:to_integer(Input) of
    {IntValue, <<>>} -> {ok, IntValue};
    _ -> {error, #{reason => non_integer, detail => Input, notice => autoconvertion_failed}}
  end;

tm_handle_scalar(#{}, null, _) ->
  {ok, undefined};

tm_handle_scalar(#{}, undefined, _) ->
  {ok, undefined};

tm_handle_scalar(#{}, Input, #type_mapper_type{source=value, body=ImmediateValue}) when 
  is_atom(ImmediateValue) andalso is_binary(Input) ->
  case atom_to_binary(ImmediateValue,latin1) of
    Input -> {ok, ImmediateValue};
    _ -> {ok, Input}
  end;

tm_handle_scalar(#{allow_type_convertion := true}, Input, #type_mapper_type{name=atom}) when is_binary(Input) ->
  {ok, binary_to_atom(Input,latin1)};

tm_handle_scalar(#{}, Input, #type_mapper_type{name=binary}) when is_atom(Input) ->
  {ok, atom_to_binary(Input,latin1)};

tm_handle_scalar(#{allow_type_convertion := true}, Input, #type_mapper_type{name=boolean}) ->
  case Input of
    <<"true">> -> {ok, true};
    <<"false">> -> {ok, false};
    _ -> {ok, Input}
  end;

tm_handle_scalar(_, Input, _) ->
  {ok, Input}.







tm_handle_record_field(_,_,Value,_) ->
  {ok, Value}.

tm_handle_close_record(_, _, Input, Record) ->
  {ok, Input, Record}.

