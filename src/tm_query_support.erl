-module(tm_query_support).
-include("../include/type_mapper.hrl").

-export([tm_handle_scalar/3, tm_handle_record_field/4, tm_handle_close_record/4]).


tm_handle_scalar(_, #{} = Input, #type_mapper_type{name = Int}) 
  when Int == integer; Int == non_neg_integer; Int == number; Int == range; Int == pos_integer ->
  case maps:to_list(Input) of
    [{Compare,Strval}] when Compare == '$gt'; Compare == '$lt'; Compare == '$gte'; Compare == '$lte' ->
      case string:to_integer(Strval) of
        {IntValue, <<>>} ->
          {ok, #{Compare => IntValue}};
        _ -> 
          {error, #{reason => non_integer, detail => Input, notice => autoconvertion_failed}}
      end;
    _ ->
      {error, #{reason => non_comparable, detail => Input, notice => autoconvertion_failed}}
  end;

tm_handle_scalar(#{module := Module} = Opts, Input, #type_mapper_type{} = Type) when is_list(Input) ->
  ParsedInput = lists:map(fun(I) ->
    Callbacks = [{tm_converter,Opts#{allow_type_convertion => true}},{tm_validator,Opts}],
    case tm_visitor:visit(#{module => Module, type => Type, callbacks => Callbacks, output => record}, I) of
      {ok, V} -> V;
      {error, _E} -> I
    end
  end, Input),
  {ok, ParsedInput};

tm_handle_scalar(_, Input, _) ->
  {ok, Input}.




tm_handle_record_field(_,_,Value,_) ->
  {ok, Value}.

tm_handle_close_record(_, _, Input, Record) ->
  {ok, Input, Record}.
