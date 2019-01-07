open Field
open Type
open Expression

type record = Record of system_type * string

val record_of: system_type -> variable -> record
val type_of: record -> system_type 	
val name_of: record -> string
val string_of: record -> string