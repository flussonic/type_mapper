

-record(type_mapper_field, {
  name,
  index,
  default,
  default_type,
  types
}).

-record(type_mapper_type, {
  name,
  source :: user | system | value | remote,
  body = []
}).
