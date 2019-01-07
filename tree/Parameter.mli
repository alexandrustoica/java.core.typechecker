open Type

type parameter = Parameter of system_type * string

val string_of_parameter: parameter -> string
val string_of_parameters: parameter list -> string