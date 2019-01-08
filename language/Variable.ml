
include VariableType

let string_of = function
	| KNull -> "Null"
	| KInt value -> string_of_int value
	| KFloat value -> string_of_float value
	| KBool value -> string_of_bool value
	| VarWithName value -> value
	| VarWithField (variable, field) -> variable ^ "." ^ field
