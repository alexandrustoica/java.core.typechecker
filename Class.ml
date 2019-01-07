open Method
open Field

type class_declaration =
	| InheritanceDeclaration of (string * string *
	field_declaration list * method_declaration list)
	| ClassDeclaration of (string *
	field_declaration list * method_declaration list)


let string_of_class 
	(cls: class_declaration): string =
	match cls with
	| InheritanceDeclaration (name, super, fields, methods) ->
			"class " ^ name ^ " extends " ^ super ^ "{\n" ^
			List.fold_left (fun acc it -> (string_of_field it) ^ "\n" ^ acc) "" fields ^
			List.fold_left (fun acc it -> acc ^ "\n" ^ (string_of_method it)) "" methods ^
			"\n}"
	| ClassDeclaration (name, fields, methods) ->
			"class " ^ name ^ "{\n" ^
			List.fold_left (fun acc it -> (string_of_field it) ^ "\n" ^ acc) "" fields ^
			List.fold_left (fun acc it -> acc ^ "\n" ^ (string_of_method it)) "" methods ^
			"\n}"
