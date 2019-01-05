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
	
	let to_string (methods: method_declarations) = match methods with
	| MethodDeclarations values -> 
		let head = List.hd values in
		match head with 
		| MethodDeclaration (typ, functionName, params, expression) -> 
			string_of_type typ
	
	let to_string (cls: class_declaration) = match cls with
	| ClassDeclaration (name, super, fields, methods) -> 
		to_string methods
	
	let to_string (cls: class_declarations) = match cls with
	| ClassDeclarations classes -> to_string (List.hd classes)
	
	let to_string (prog: program) = match prog with
	| Program cls -> to_string cls

end