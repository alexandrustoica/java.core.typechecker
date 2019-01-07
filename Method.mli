open SystemType
open Expression
open Parameter

type method_declaration =
	|	MethodDeclaration of 
	system_type * string * parameter list * expression
	
val string_of_method: method_declaration -> string
