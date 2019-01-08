
type method_declaration =
	|	MethodDeclaration of 
	Type.system_type * string * 
	Parameter.parameter list * 
	Expression.expression

let string_of = function
	| MethodDeclaration (typ, name, params, expression) ->
			(Type.string_of_type typ) ^ " " ^ name ^
			(Parameter.string_of_parameters params) ^
			(Expression.string_of_expression expression)
