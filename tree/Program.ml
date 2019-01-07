open Class
open Type


type program = Program of class_declaration list

let string_of_program 
	(prog: program) : string = 
		match prog with
	| Program classes ->
		 List.fold_left 
		(fun acc it -> acc ^ "\n" ^ it) "" 
		(List.map string_of_class classes)

let user_types_in (prog: program) : system_type list =
	match prog with
	| Program classes ->
		(List.map (fun it -> UserDefinedType(Class.name_of_class it)) classes) @ [UserDefinedType("Object")]
	 