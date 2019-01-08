open Method
open Field

type class_declaration =
	| InheritanceDeclaration of (string * string *
	field_declaration list * method_declaration list)
	| ClassDeclaration of (string *
	field_declaration list * method_declaration list)

let fields_of_class 
	(cls: class_declaration) : 
	field_declaration list =
	match cls with
	| ClassDeclaration (_, fields, _) -> fields
	| InheritanceDeclaration(_, _, fields, _) -> fields

let methods_of_class
	(cls: class_declaration) : 
	method_declaration list =
		match cls with
		| ClassDeclaration (_, _, methods) -> methods
		| InheritanceDeclaration (_, _, _, methods) -> methods


let name_of_class
	(cls: class_declaration) : string =
		match cls with
		| ClassDeclaration (name, _, _) -> name
		| InheritanceDeclaration (name, _, _, _) -> name
	
let super_of_class
	(cls: class_declaration) : string =
		match cls with
		| ClassDeclaration (_, _, _) -> "Object"
		| InheritanceDeclaration (_, super, _, _) -> super

let string_of_class 
	(cls: class_declaration): string =
	match cls with
	| InheritanceDeclaration (name, super, fields, methods) ->
			"class " ^ name ^ " extends " ^ super ^ "{\n" ^
			List.fold_left (fun acc it -> (Field.string_of it) ^ "\n" ^ acc) "" fields ^
			List.fold_left (fun acc it -> acc ^ "\n" ^ (Method.string_of it)) "" methods ^
			"\n}"
	| ClassDeclaration (name, fields, methods) ->
			"class " ^ name ^ "{\n" ^
			List.fold_left (fun acc it -> (Field.string_of it) ^ "\n" ^ acc) "" fields ^
			List.fold_left (fun acc it -> acc ^ "\n" ^ (Method.string_of it)) "" methods ^
			"\n}"
