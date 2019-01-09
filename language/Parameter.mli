
type parameter = Parameter of Type.system_type * string

val string_of_parameter: parameter -> string
val string_of_parameters: parameter list -> string

val type_of: parameter -> Type.system_type
val name_of: parameter -> string
