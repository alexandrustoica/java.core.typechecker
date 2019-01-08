
let fields_for typ program =
	match program with
	| Program.Program classes ->
			let equal_by_name = fun it -> (Class.name_of_class it) = typ in
			classes
			|> List.find_all equal_by_name
			|> List.map Class.fields_of_class
			|> List.flatten

let fields_for typ in_program =
	match typ with
	| Type.UserDefinedType value -> fields_for value in_program
	| _ -> []

let fields_in program for_class =
	let fields_for_type t = fields_for t program in
	match for_class with
	| "Object" -> []
	| _ ->
			let class_type = Type.UserDefinedType(for_class) in
			let related = (RelatedType.related_with class_type program) in
			let fields = class_type :: related in
			fields |> List.map fields_for_type |> List.flatten