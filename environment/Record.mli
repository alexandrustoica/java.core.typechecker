
type record = Record of Type.system_type * string

val record_of: Type.system_type -> Variable.variable -> record
val type_of: record -> Type.system_type 	
val name_of: record -> string
val string_of: record -> string
