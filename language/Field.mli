
type field_declaration =
	| FieldDeclaration of Type.system_type * string
	
val type_of: field_declaration -> Type.system_type
val name_of: field_declaration -> string
val string_of: field_declaration -> string

val find_by: string -> field_declaration list -> field_declaration
