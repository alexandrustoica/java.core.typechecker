open Type
open Record 

type environment = Environment of record list

val string_of: environment -> string
val find_in: environment -> string -> system_type
val insert_in: environment -> record -> environment
val records_of: environment -> record list