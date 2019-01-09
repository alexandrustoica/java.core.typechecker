

type method_declaration =	
	| MethodDeclaration of 
	Type.system_type * string * 
	Parameter.parameter list * 
	Expression.expression
	
val string_of: method_declaration -> string
val parameters_of: method_declaration -> Parameter.parameter list
val expression_of: method_declaration -> Expression.expression 
val type_of: method_declaration -> Type.system_type
val name_of: method_declaration -> string 