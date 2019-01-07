open SystemType
open Expression

type record = Record of (system_type * string)
type environment = Environment of record list

val type_of: record -> system_type 	
val name_of: record -> string
val string_of: environment -> string

val find_in: environment -> string -> system_type
val insert_in: environment -> record -> environment
val to_record: system_type -> variable -> record