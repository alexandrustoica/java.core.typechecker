
type parameter = Parameter of Type.system_type * string

let string_of_parameter = function
	| Parameter (typ, name) -> 
		(Type.string_of_type typ) ^ " " ^ name

let string_of_parameters = function
	| [] -> "()"
	| h :: t ->
		let compose = fun acc it -> acc ^ ", " ^ (string_of_parameter it) in
		let head = (string_of_parameter h) in
		let result = t |> List.fold_left compose head in
		"(" ^ result ^ ")"
		
let type_of = function Parameter (typ, _) -> typ

let name_of = function Parameter (_, name) -> name
