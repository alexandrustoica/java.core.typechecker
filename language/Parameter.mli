
type parameter = Parameter of Type.system_type * string

val string_of_parameter: parameter -> string
val string_of_parameters: parameter list -> string