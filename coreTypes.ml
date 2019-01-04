type core_type =
	| CoreInt
	| CoreString
	| CoreBool 
	| CoreFloat
	| CoreUnit;;

type system_type =
	| CoreType of core_type 
	| UserDefinedType of string;;

type field_declaration = 
	{systemType : system_type; name: string};;

type parameter =
	| Parameter of system_type * string;;

type method_declaration = {
	systemType: system_type; 
	name: string; 
	parameters: parameter list; 
	expression: string
};;

type class_declaration =
	| ClassDeclaration of string * string * 
	field_declaration list * method_declaration list;;

type program = Program of class_declaration list;;

type ast = 
	| Program of program
	| ClassDeclaration of class_declaration
	| MethodDeclaration of method_declaration
	| Parameter of parameter
	| SystemType of system_type
