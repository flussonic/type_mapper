-module(tm_logger).
-include("../include/type_mapper.hrl").


-export([tm_handle_scalar/3, tm_handle_record_field/4, tm_handle_close_record/4]).



tm_handle_scalar(_, Input, Type) ->
  io:format("tm_handle_scalar(~p,~p)\n", [Input, Type]),
  {ok, Input}.



tm_handle_record_field(_, Class, ExtractedValue, #type_mapper_field{} = Field) ->
  io:format("tm_handle_record_field(~0p,~0p,~0p)\n", [Class, ExtractedValue, Field]),
  {ok, ExtractedValue}.


tm_handle_close_record(_, _, Input, Record) ->
  {ok, Input, Record}.
