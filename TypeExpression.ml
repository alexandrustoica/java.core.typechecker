open Expression.Expression
open SystemType.Type
open Tree.Tree
open FieldList.FieldList
open Field.Field
open Environment.Environment

let type_of_variable
	(program: program)
	(environment: environment)
 	(variable: variable): system_type =
	match variable with
	| KNull -> NullType
	| KInt _ -> PrimitiveType(CoreInt)
	| KFloat _ -> PrimitiveType(CoreFloat)
	| KBool _ -> PrimitiveType(CoreBool)
	| VarWithName name -> find_in environment name
	| VarWithField (name, field) -> 
			let result = List.find (fun it -> match it with 
			| FieldDeclaration (_, name) -> name = field)
			(fields_in program (string_of_type (find_in environment name)))
			in match result with
			| FieldDeclaration (typ, _) -> typ


let field_to_record (field: field_declaration): (system_type * string) =
	match field with
	| FieldDeclaration (typ, name) -> (typ, name)

let fields_to_records 
	(fields: field_declaration list):
	(system_type * string) list =
		List.map (fun it -> (field_to_record it)) fields 

exception ErrorInvalidTypeEq

let rec type_of_expression 
	(program: program)
	(environment: environment)
	(expression: expression): system_type =
	let _ = print_endline (string_of environment)
	in match expression with
	| Void -> PrimitiveType(CoreUnit)
	| Var variable -> (type_of_variable program environment variable)  
	| LocalVar (typ, var, expr) ->
			type_of_expression program (insert_in environment (to_record typ var)) expr
	| Assign (var, expr) ->
			if (SubType.is_subtype program 
			(type_of_variable program environment var) 
			(type_of_expression program environment expr)) 
			then PrimitiveType(CoreUnit) else (raise ErrorInvalidTypeEq) 
	| _ -> PrimitiveType(CoreInt)
	
let type_of (expression: expression) (_in: program): system_type = 
	type_of_expression _in (Environment([])) expression	
	
	
	