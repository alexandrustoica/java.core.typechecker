
type field_declaration =
	| FieldDeclaration of Type.system_type * string
	
let type_of = function
	| FieldDeclaration (typ, _) -> typ

let name_of = function
	| FieldDeclaration (_, name) -> name

let string_of = function
	| FieldDeclaration (typ, name) -> 
		(Type.string_of_type typ) ^ " " ^ name ^ ";"

let find_by name within =
	List.find (fun it -> (name_of it) = name) within
