open Class
open Type

type program = Program of class_declaration list

val string_of_program: program -> string

val user_types_in: program -> system_type list  

(** Checks if a given class by name is defined in a given program *)
val exists: string -> program -> bool