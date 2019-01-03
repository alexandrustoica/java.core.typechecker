open CoreTypes

module Printer = struct	


let print_field field =
	match field with
	| FieldDeclaration (coreType, fieldName) ->
			match coreType with
			| CoreString (value) -> "String " ^ fieldName ^ " = \"" ^ value ^ "\";"
			| CoreFloat (value) -> "Float " ^ fieldName ^ " = " ^ string_of_float value ^ ";"
			| CoreBool (value) -> "Bool " ^ fieldName ^ " = " ^ string_of_bool value ^ ";"
			| CoreInt (value) -> "Int " ^ fieldName ^ " = " ^ string_of_int value ^ ";";;

let print_class_declaration declaration =
	match declaration with
	| ClassDeclaration (base, super, fields, methods) ->
			"class " ^ base ^ " extends " ^ super ^ " { " ^
			List.fold_left (fun acc field -> acc ^ "\t" ^ (print_field field) ^ "\n") "\n" fields ^
			"}" ;;

let print program =
	match program with
	| Program (classes) ->
			List.fold_left (fun acc classDeclaration ->
							acc ^ (print_class_declaration classDeclaration)) "" classes;;	

end;;

