open Type
open Expression
open Parameter

type method_declaration =	
	| MethodDeclaration of 
	Type.system_type * string * 
	Parameter.parameter list * 
	Expression.expression
	
val string_of: method_declaration -> string
