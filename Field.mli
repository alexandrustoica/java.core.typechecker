open SystemType

type field_declaration =
	| FieldDeclaration of system_type * string
	
val type_of_field: field_declaration -> system_type
val name_of_field: field_declaration -> string
val string_of_field: field_declaration -> string
