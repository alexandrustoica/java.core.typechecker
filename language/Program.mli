
type program = Program of Class.class_declaration list

val string_of: program -> string
val user_types_in: program -> Type.system_type list  

(** Checks if a given class by name is defined in a given program *)
val exists: string -> program -> bool
