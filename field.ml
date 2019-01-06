open SystemType.Type

module Field = struct
	
	type field_declaration =
		| FieldDeclaration of system_type * string
	
	let type_of_field field = match field with
		| FieldDeclaration (typ, _) -> typ
	
	let name_of_field field = match field with
		| FieldDeclaration (_, name) -> name
	
end