open Typ.Type
open Expression

module AST = struct
	
	type parameter = Parameter of system_type * string
	
	type program = Program of class_declarations
	
	and class_declarations =
			ClassDeclarations of class_declaration list
	
	and class_declaration =
			ClassDeclaration of (string * string *
		field_declarations * method_declarations)
	
	and field_declarations =
			FieldDeclarations of field_declaration list
	
	and field_declaration =
			FieldDeclaration of system_type * string
	
	and method_declarations =
			MethodDeclarations of method_declaration list
	
	and method_declaration =
			MethodDeclaration of system_type * string * parameter list *
		Expression.expression
	
	let string_of_parameter (param: parameter) = match param with
	| Parameter (typ, name) -> (string_of_type typ) ^ " " ^ name
	
	let string_of_parameters (params: parameter list) = match params with
	| [] -> "()"
	| h::t -> "(" ^ List.fold_left (fun acc it -> acc ^ ", " ^ (string_of_parameter it))
	(string_of_parameter h) t ^ ")"
	
	let string_of_method (meth: method_declaration) = match meth with
	| MethodDeclaration (typ, name, params, expression) ->
						(string_of_type typ) ^ " " ^ name ^ 
						(string_of_parameters params) ^
						(Expression.string_of_expression expression)
						
	let string_of_methods (methods: method_declarations) = match methods with
	| MethodDeclarations values -> 
		List.fold_left (fun acc it -> acc ^ "\n" ^ (string_of_method it)) "" values 

	let string_of_field (field: field_declaration) = match field with
	| FieldDeclaration (typ, name) -> (string_of_type typ) ^ " " ^ name ^ ";"
	
	let string_of_fields (fields: field_declarations) = match fields with
	| FieldDeclarations values -> List.fold_left (fun acc it -> (string_of_field it) ^ "\n" ^ acc) "" values
	
	let string_of_class (cls: class_declaration) = match cls with
		| ClassDeclaration (name, super, fields, methods) ->
			"class " ^ name ^ " extends " ^ super ^ "{\n" ^
				(string_of_fields fields) ^
				(string_of_methods methods) ^ 
			 "\n}"
	
	let string_of_classes (cls: class_declarations) = match cls with
		| ClassDeclarations classes -> 
			List.fold_left (fun acc it -> acc ^ "\n" ^ (string_of_class it)) "" classes
	
	let string_of_program (prog: program) = match prog with
		| Program cls -> string_of_classes cls
	
end