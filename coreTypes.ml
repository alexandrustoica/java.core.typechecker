type core_type =
	| CoreInt of int
	| CoreFloat of float
	| CoreBool of bool
	| CoreString of string;;

type field_declaration =
	| FieldDeclaration of core_type * string;;

type method_declarations =
	| MethodDeclarations of string list;;

type class_declaration =
	| ClassDeclaration of string * string *
	field_declaration list * method_declarations;;

type program = Program of class_declaration list;;