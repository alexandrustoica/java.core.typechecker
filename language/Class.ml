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

let find_method_by name in_class =
	let methods = methods_of_class in_class
	and eq = fun it -> (Method.name_of it) = name in
	methods |> List.find_opt eq

let rec duplicates in_list based_on acc =
	let eq h = (fun it -> (based_on it) = (based_on h)) in
	match in_list with
	| [] -> acc
	| h :: t ->
			match (List.find_opt (eq h) t) with
			| Some x -> duplicates t based_on (x::acc)
			| None -> duplicates t based_on acc

let find_duplicates based_on in_list = duplicates in_list based_on []

let duplications in_class =
	let methods = (methods_of_class in_class)
	|> find_duplicates Method.name_of
	and fields = (fields_of_class in_class)
	|> find_duplicates Field.name_of in
	(fields, methods)
