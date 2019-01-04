open CoreTypes

module Printer = struct
	
	
	let type_to_string core_type =
		match core_type with
		| CoreInt -> "Int"
		| CoreString -> "String"
		| CoreBool -> "Bool"
		| CoreFloat -> "Float"
		| CoreUnit -> "Unit";;

	let system_type_to_string = " ";;
	
			(*		

	let print_user_defined_type systemType fieldName =
		match systemType with
		| UserDefinedType (className) -> className ^ " " ^ fieldName ^ ";"
		| CoreType (coreType) -> (type_to_string coreType) ^ " " ^ fieldName;;

	let print_field field =
		match field with
		| (systemType, fieldName) ->
			 print_user_defined_type systemType fieldName;;
	
	
	let print_method methodDeclaration =
		match methodDeclaration with
		| (returnType, name, parameters, expression) -> 
			"\t" ^ "test" ^ " " ^ name ^ "(" ^ ")";;
			
		
	let print_class_declaration declaration =
		match declaration with
		| ClassDeclaration (base, super, fields, methods) ->
				"class " ^ base ^ " extends " ^ super ^ " { " ^ "test";;
			(*	List.fold_left (fun acc field -> acc ^ "\t" ^ 
				(print_field field) ^ "\n") "\n" fields ^
				List.fold_left (fun acc m -> acc ^ "\t" ^ 
				(print_method m) ^ "\n") "\n" methods ^
				"}" ;;
			*)

	let print program =
		match program with
		| Program (classes) ->
				List.fold_left (fun acc classDeclaration ->
								acc ^ (print_class_declaration classDeclaration)) "" classes;;
	
	*)
	
end;;
