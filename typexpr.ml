open Expression.Expression
open Typ.Type
open Ast.AST


let type_of_variable (variable: variable) =
	match variable with
	| KNull -> NullType
	| KInt _ -> PrimitiveType(CoreInt)
	| KFloat _ -> PrimitiveType(CoreFloat)
	| KBool _ -> PrimitiveType(CoreBool)
	| _ -> NullType 

let to_record 
	(typ: system_type)
	(variable: variable): (system_type * string) =
	match variable with
	| VarWithName name -> (typ, name)
	| _ -> (typ, "CheckThisPoint") (*REFACTOR*)

let string_of_record (record: system_type * string): string =
	match record with
	| (typ, str) -> (string_of_type typ) ^ ":" ^ str

let print_environment (env: (system_type * string) list) =
	List.fold_left (fun acc it -> acc ^ it) "" 
	(List.map string_of_record env)

let rec type_of_expression 
	(environment: (system_type * string) list)
	(expression: expression) =
	let x = print_endline (print_environment environment)
	in match expression with
	| Void -> PrimitiveType(CoreUnit)
	| Var variable -> (type_of_variable variable)  
	| LocalVar (typ, var, expr) ->
			type_of_expression ((to_record typ var)::environment) expr
	| _ -> PrimitiveType(CoreInt)
	
