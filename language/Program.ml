
type program = Program of Class.class_declaration list

let string_of = function
	| Program classes -> classes
			|> List.map Class.string_of_class
			|> List.fold_left (fun acc it -> acc ^ "\n" ^ it) ""

let user_types_in = function
	| Program classes ->
			Type.UserDefinedType("Object") :: (classes |>
				List.map (fun it -> Type.UserDefinedType(Class.name_of_class it)))

let exists class_name within =
	(user_types_in within)
	|> List.map Type.string_of_type
	|> List.exists (fun it -> it == class_name)

let classes_of = function Program classes -> classes

let find_class typ within =
	let name = (Type.string_of_type typ) in
	let eq = fun it -> (Class.name_of_class it) = name
	and classes = classes_of within in
	classes |> List.find_opt eq
	
let duplications = function
	| Program classes -> 
		let class_name it = Class.name_of_class it in
		let eq x = fun it -> (class_name it) = (class_name x) in
		let rec duplicated_classes classes acc =
			match classes with
			| [] -> acc
			| h :: t -> 
				match (List.find_opt (eq h) t) with
				| None -> duplicated_classes t acc
				| Some cls -> duplicated_classes t (cls::acc) in
		duplicated_classes classes []