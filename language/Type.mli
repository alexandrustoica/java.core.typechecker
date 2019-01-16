
type typ = CoreInt | CoreBool | CoreUnit | CoreFloat

type system_type =
	| PrimitiveType of typ
	| UserDefinedType of string
	| NullType

val compare: typ -> typ -> bool
val compare: system_type -> system_type -> bool

val are_equal: system_type list -> bool

val string_of_type: typ -> string
val string_of_type: system_type -> string
val string_of_types: system_type list -> string

val is_user_defined: system_type list -> bool
