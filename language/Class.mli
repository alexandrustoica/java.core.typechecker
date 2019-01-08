open Method
open Field

type class_declaration =
	| InheritanceDeclaration of (string * string *
	field_declaration list * method_declaration list)
	| ClassDeclaration of (string *
	field_declaration list * method_declaration list)

val string_of_class: class_declaration -> string

val fields_of_class: class_declaration -> field_declaration list
val methods_of_class: class_declaration -> method_declaration list
val name_of_class: class_declaration -> string
val super_of_class: class_declaration -> string