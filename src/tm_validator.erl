-module(tm_validator).
-include("../include/type_mapper.hrl").

-export([tm_handle_scalar/3, tm_handle_record_field/4, tm_handle_close_record/4]).


tm_handle_scalar(#{allow_forced_undefined := true}, undefined, _) ->
  {ok, undefined};

tm_handle_scalar(#{check_numbers := true}, Input, #type_mapper_type{name = non_neg_integer}) ->
  if
    is_integer(Input) andalso Input >= 0 -> {ok, Input};
    is_integer(Input) -> {error, #{reason => negative_integer}};
    true -> {error, #{reason => non_integer}}
  end;

tm_handle_scalar(#{check_numbers := true}, Input, #type_mapper_type{name = pos_integer}) ->
  if
    is_integer(Input) andalso Input > 0 -> {ok, Input};
    is_integer(Input) -> {error, #{reason => non_pos_integer}};
    true -> {error, #{reason => non_integer}}
  end;

tm_handle_scalar(#{check_numbers := true}, Input, #type_mapper_type{name = integer}) ->
  if
    is_integer(Input) -> {ok, Input};
    true -> {error, #{reason => non_integer}}
  end;

tm_handle_scalar(#{check_numbers := true}, Input, #type_mapper_type{name = number}) ->
  if
    is_number(Input) -> {ok, Input};
    true -> {error, #{reason => non_number}}
  end;

tm_handle_scalar(#{check_numbers := true}, Input, #type_mapper_type{name = range, body = [From,To]}) ->
  case Input of
    _ when From =< Input andalso Input =< To -> {ok, Input};
    _ -> {error, #{reason => out_of_range, detail => Input}}
  end;

tm_handle_scalar(#{}, Input, #type_mapper_type{source = value, body = ImmediateValue}) ->
  case ImmediateValue of
    Input -> {ok, Input};
    _ -> {error, #{reason => non_matching_value, detail => Input}}
  end;

tm_handle_scalar(#{}, Input, #type_mapper_type{name=boolean}) ->
  case Input of
    true -> {ok, Input};
    false -> {ok, Input};
    _ -> {error, #{reason => non_boolean_value}} % , detail => Input
  end;

tm_handle_scalar(#{}, Input, #type_mapper_type{name=binary}) ->
  case Input of
    <<_/binary>> -> {ok, Input};
    _ -> {error, #{reason => non_binary, detail => Input}}
  end;

tm_handle_scalar(#{}, Input, #type_mapper_type{name=atom}) ->
  case Input of
    _ when is_atom(Input) -> {ok, Input};
    _ -> {error, #{reason => non_atom, detail => Input}}
  end;

tm_handle_scalar(#{}, _Input, {record, _RecName}) ->
  {error, #{reason => non_map_input}}; % , detail => Input

tm_handle_scalar(#{}, _Input, {map, _, _RecName}) ->
  {error, #{reason => non_map_input}}; % , detail => Input

tm_handle_scalar(#{}, _Input, {list, _}) ->
  {error, #{reason => non_list}}; % , detail => Input

tm_handle_scalar(#{validators := #{} = Validators}, Input, #type_mapper_type{name=TName, source=user}) ->
  case maps:get(TName, Validators, undefined) of
    undefined ->
      {ok, Input};
    Fun ->
      case Fun(Input) of
        {ok, Data} -> {ok, Data};
        {error, #{} = E} -> {error, E}
      end
  end;

tm_handle_scalar(_Arg, Input, _Type) ->
  {ok, Input}.




tm_handle_record_field(#{allow_miss_mandatory := true}, lack, _, #type_mapper_field{}) ->
  skip;

tm_handle_record_field(#{}, lack, _, #type_mapper_field{}) ->
  {error, #{reason => lacks_mandatory}};

tm_handle_record_field(_,_,Value,_) ->
  {ok, Value}.



tm_handle_close_record(_, _, Input, Record) ->
  if 
    Input == #{} orelse Input == [] -> {ok, Input, Record};
    true -> {error, #{reason => extra_input, unparsed => Input}}
  end.


