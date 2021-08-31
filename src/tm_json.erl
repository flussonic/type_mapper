-module(tm_json).
-include("../include/type_mapper.hrl").

-export([tm_handle_scalar/3, tm_handle_record_field/4, tm_handle_close_record/4]).




tm_handle_scalar(_, Input, _) ->
  {ok, Input}.



tm_handle_record_field(#{output := map}, _, Input, #type_mapper_field{}) when is_pid(Input) ->
  skip;

tm_handle_record_field(#{skip_map_defaults := true}, default, _X, _Y) ->
  skip;

tm_handle_record_field(#{skip_map_defaults := false}, default, undefined, _) ->
  skip;

tm_handle_record_field(#{skip_map_defaults := false}, default, Value, _) ->
  {ok, Value};

% tm_handle_record_field(#{skip_map_defaults := trivial}, default, Value, _) when Value =/= undefined ->
tm_handle_record_field(#{}, default, Value, _) when Value =/= undefined andalso Value =/= #{} ->
  {ok, Value};

tm_handle_record_field(_V, default, _X, _Y) ->
  skip;

tm_handle_record_field(_, _, ExtractedValue, _) ->
  {ok, ExtractedValue}.



tm_handle_close_record(_, _, Input, Record) ->
  {ok, Input, Record}.

