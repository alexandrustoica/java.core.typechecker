open Typ.Type
open Expression

module AST = struct
	
	type parameter = Parameter of system_type * string
	
	type program = Program of class_declaration list
	
	and class_declaration =
		| InheritanceDeclaration of (string * string *
		field_declaration list * method_declaration list)
		| ClassDeclaration of (string * 
		field_declaration list * method_declaration list)
	
	and field_declaration =
		|	FieldDeclaration of system_type * string
	
	and method_declaration =
		|	MethodDeclaration of system_type * string *
		parameter list * Expression.expression
	
	let string_of_parameter (param: parameter) = match param with
		| Parameter (typ, name) -> (string_of_type typ) ^ " " ^ name
	
	let string_of_parameters (params: parameter list) = match params with
		| [] -> "()"
		| h:: t -> "(" ^ List.fold_left (fun acc it -> acc ^ ", " ^ (string_of_parameter it))
					(string_of_parameter h) t ^ ")"
	
	let string_of_method (meth: method_declaration) = match meth with
		| MethodDeclaration (typ, name, params, expression) ->
				(string_of_type typ) ^ " " ^ name ^
				(string_of_parameters params) ^
				(Expression.string_of_expression expression)
	
	let string_of_field (field: field_declaration) = match field with
		| FieldDeclaration (typ, name) -> (string_of_type typ) ^ " " ^ name ^ ";"
	
	let string_of_class (cls: class_declaration) = match cls with
	| InheritanceDeclaration (name, super, fields, methods) ->
			"class " ^ name ^ " extends " ^ super ^ "{\n" ^
				List.fold_left (fun acc it -> (string_of_field it) ^ "\n" ^ acc) "" fields ^
				List.fold_left (fun acc it -> acc ^ "\n" ^ (string_of_method it)) "" methods ^
				"\n}"
	| ClassDeclaration (name, fields, methods) ->
				"class " ^ name ^ "{\n" ^
				List.fold_left (fun acc it -> (string_of_field it) ^ "\n" ^ acc) "" fields ^
				List.fold_left (fun acc it -> acc ^ "\n" ^ (string_of_method it)) "" methods ^
				"\n}"
	
	let string_of_program (prog: program) = match prog with
		| Program classes -> List.fold_left (fun acc it -> acc ^ "\n" ^ (string_of_class it)) "" classes
	
end