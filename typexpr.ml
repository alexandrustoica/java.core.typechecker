open Expression.Expression
open SystemType.Type
open Ast.AST
open Fieldlist.FieldList
open Field.Field

let find_type_of (var: string) (_in: (system_type * string) list): system_type =
	let record = List.find (fun it -> match it with
	| (typ, name) -> name = var) _in
	in match record with 
	| (typ, _) -> typ

let type_of_variable
	(program: program)
	(environment: (system_type * string) list)
 	(variable: variable) =
	match variable with
	| KNull -> NullType
	| KInt _ -> PrimitiveType(CoreInt)
	| KFloat _ -> PrimitiveType(CoreFloat)
	| KBool _ -> PrimitiveType(CoreBool)
	| VarWithName name -> find_type_of name environment
	| VarWithField (name, field) -> 
			let result = List.find (fun it -> match it with 
			| FieldDeclaration (_, name) -> name = field)
			(fields_in program (string_of_type (find_type_of name environment)))
			in match result with
			| FieldDeclaration (typ, _) -> typ

let to_record 
	(typ: system_type)
	(variable: variable): (system_type * string) =
	match variable with
	| VarWithName name -> (typ, name)
	| _ -> (typ, "CheckThisPoint") (*REFACTOR*)

let field_to_record (field: field_declaration): (system_type * string) =
	match field with
	| FieldDeclaration (typ, name) -> (typ, name)

let fields_to_records 
	(fields: field_declaration list):
	(system_type * string) list =
		List.map (fun it -> (field_to_record it)) fields 

let string_of_record (record: system_type * string): string =
	match record with
	| (typ, str) -> (string_of_type typ) ^ ":" ^ str

let print_environment (env: (system_type * string) list) =
	List.fold_left (fun acc it -> acc ^ it) "" 
	(List.map string_of_record env)

let rec type_of_expression 
	(program: program)
	(environment: (system_type * string) list)
	(expression: expression) =
	let _ = print_endline (print_environment environment)
	in match expression with
	| Void -> PrimitiveType(CoreUnit)
	| Var variable -> (type_of_variable program environment variable)  
	| LocalVar (typ, var, expr) ->
			type_of_expression program ((to_record typ var)::environment) expr
	| _ -> PrimitiveType(CoreInt)
	
