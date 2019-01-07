open SystemType

type field_declaration =
	| FieldDeclaration of system_type * string
	
let type_of_field field = match field with
	| FieldDeclaration (typ, _) -> typ

let name_of_field field = match field with
	| FieldDeclaration (_, name) -> name

let string_of_field 
	(field: field_declaration): string =
	match field with
	| FieldDeclaration (typ, name) -> 
		(string_of_type typ) ^ " " ^ name ^ ";"
