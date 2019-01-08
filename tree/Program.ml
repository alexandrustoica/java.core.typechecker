open Class
open Type


type program = Program of class_declaration list

let string_of_program (prog: program) : string = match prog with
	| Program classes -> classes 
		|> List.map Class.string_of_class 
		|> List.fold_left (fun acc it -> acc ^ "\n" ^ it) ""
		
let user_types_in (prog: program) : system_type list = match prog with
	| Program classes -> 
		UserDefinedType("Object") :: (classes |> 
		List.map (fun it -> UserDefinedType(Class.name_of_class it)))
	 
let exists (class_name: string) (_in: program): bool = 
		(user_types_in _in)
		|> List.map Type.string_of_type
		|> List.exists (fun it -> it == class_name)
