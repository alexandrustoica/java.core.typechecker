open Type
open Expression
open Parameter

type method_declaration =
	|	MethodDeclaration of 
	system_type * string * parameter list * expression


let string_of_method (meth: method_declaration): string =
	match meth with
	| MethodDeclaration (typ, name, params, expression) ->
			(string_of_type typ) ^ " " ^ name ^
			(string_of_parameters params) ^
			(string_of_expression expression)
