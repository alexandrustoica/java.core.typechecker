open Class

type program = Program of class_declaration list

let string_of_program 
	(prog: program) : string = 
		match prog with
	| Program classes ->
		 List.fold_left 
		(fun acc it -> acc ^ "\n" ^ it) "" 
		(List.map string_of_class classes)
