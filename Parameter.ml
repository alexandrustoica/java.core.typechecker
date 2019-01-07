open SystemType

type parameter = Parameter of system_type * string

let string_of_parameter
		(param: parameter): string =
	match param with
	| Parameter (typ, name) ->
			(string_of_type typ) ^ " " ^ name

let string_of_parameters
		(params: parameter list): string =
	match params with
	| [] -> "()"
	| h:: t -> "(" ^ (List.fold_left
					(fun acc it -> acc ^ ", " ^ (string_of_parameter it))
					(string_of_parameter h) t) ^ ")"
