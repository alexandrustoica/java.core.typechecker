
type environment = Environment of Record.record list

val string_of: environment -> string
val find_in: environment -> string -> Type.system_type
val insert_in: environment -> Record.record -> environment
val records_of: environment -> Record.record list
