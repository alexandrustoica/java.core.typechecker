
type program = Program of Class.class_declaration list

val classes_of: program -> Class.class_declaration list


val find_class: Type.system_type -> program -> Class.class_declaration option

val string_of: program -> string
val user_types_in: program -> Type.system_type list  

(** Checks if a given class by name is defined in a given program *)
val exists: string -> program -> bool

val duplications: program -> Class.class_declaration list

val declared_methods_of: Class.class_declaration ->
	program -> Method.method_declaration list
	
val declared_fields_of: Class.class_declaration -> 
	program -> Field.field_declaration list