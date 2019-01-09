
type class_declaration =
	| InheritanceDeclaration of (string * string *
	Field.field_declaration list * Method.method_declaration list)
	| ClassDeclaration of (string *
	Field.field_declaration list * Method.method_declaration list)

val string_of_class: class_declaration -> string

val fields_of_class: class_declaration -> Field.field_declaration list
val methods_of_class: class_declaration -> Method.method_declaration list
val name_of_class: class_declaration -> string
val super_of_class: class_declaration -> string


val find_method_by: string -> class_declaration ->
	 Method.method_declaration option