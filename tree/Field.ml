open Type

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


let find_by (name: string) (_in: field_declaration list): field_declaration =
	List.find (fun it -> (name_of_field it) = name) _in