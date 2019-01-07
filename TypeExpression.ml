open Expression
open Type
open Program
open ClassFields
open Field
open Environment
open Record 


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
exception ErrorInvalidNewType

let rec type_of_expression
		(program: program)
		(environment: environment)
		(expression: expression): system_type =
	let _ = print_endline (Environment.string_of environment)
	in match expression with
	| Void -> PrimitiveType(CoreUnit)
	| Var variable -> (type_of_variable program environment variable)
	| LocalVar (typ, var, expr) ->
			type_of_expression program (insert_in environment (record_of typ var)) expr
	| Assign (var, expr) ->
			if (RelatedType.is_related
					(type_of_variable program environment var)
					(type_of_expression program environment expr) program)
			then PrimitiveType(CoreUnit) else (raise ErrorInvalidTypeEq)
	| Compose (lexpr, rexpr) -> (type_of_expression program environment rexpr)
	| Operation op -> type_of_operation program environment op
	| New (typ, args) -> 
		let result = UserDefinedType(typ) in
		if (List.exists (fun it -> (string_of_type it) = typ) (Program.user_types_in program)) 
		then result else (raise ErrorInvalidNewType) 
	| _ -> PrimitiveType(CoreInt)
	
and type_of_operation
		(program: program)
		(environment: environment)
		(operation: operation): system_type =
	match operation with
	| IntOperation op -> type_of_int_operation program environment op
	| FloatOperation op -> type_of_float_operation program environment op
	| BoolOperation op -> type_of_bool_operation program environment op
	| CompareOperation op -> type_of_compare_operation program environment op

and type_of_int_operation
		(program: program)
		(environment: environment)
		(operation: int_operation): system_type =
	let type_of = type_of_expression program environment
	in let type_result = PrimitiveType(CoreInt)
	in let (==) = validate_types_eq type_result type_result 
	in match operation with
	| IntPlus (l, r) -> (type_of l) == (type_of r)
	| IntMinus (l, r) -> (type_of l) == (type_of r)
	| IntDivide (l, r) -> (type_of l) == (type_of r) 
	| IntTimes (l, r) -> (type_of l) == (type_of r)

and type_of_float_operation
		(program: program)
		(environment: environment)
		(operation: float_operation): system_type =
	let type_of = type_of_expression program environment
	in let type_result = PrimitiveType(CoreFloat)
	in let (==) = validate_types_eq type_result type_result 
	in match operation with
	| FloatPlus (l, r) -> (type_of l) == (type_of r)
	| FloatMinus (l, r) -> (type_of l) == (type_of r)
	| FloatDivide (l, r) -> (type_of l) == (type_of r)
	| FloatTimes (l, r) -> (type_of l) == (type_of r)

and type_of_bool_operation
		(program: program)
		(environment: environment)
		(operation: bool_operation): system_type =
	let type_of = type_of_expression program environment
	in let type_result = PrimitiveType(CoreBool)
	in let (==) = validate_types_eq type_result type_result 
	in match operation with
	| And (l, r) -> (type_of l) == (type_of r)
	| Or (l, r) -> (type_of l) == (type_of r)
	| Not expr -> (type_of expr) == (type_result)


and type_of_compare_operation
	(program: program)
	(environment: environment)
	(operation: compare_operation): system_type =
	let type_of = type_of_expression program environment
	in let type_result = PrimitiveType(CoreBool)
	in let validate (cmp: system_type) = validate_types_eq cmp type_result 
	in match operation with
	| LT (l, r) -> let type_of_l = (type_of l) in validate type_of_l type_of_l (type_of r)
	| GT (l, r) -> let type_of_l = (type_of l) in validate type_of_l type_of_l (type_of r)
	| LE (l, r) -> let type_of_l = (type_of l) in validate type_of_l type_of_l (type_of r)
	| GE (l, r) -> let type_of_l = (type_of l) in validate type_of_l type_of_l (type_of r)
	| EQ (l, r) -> let type_of_l = (type_of l) in validate type_of_l type_of_l (type_of r)
	| NE (l, r) -> let type_of_l = (type_of l) in validate type_of_l type_of_l (type_of r)

and validate_types_eq
		(compareWith: system_type)
		(result: system_type)
		(l: system_type)
		(r: system_type): system_type =
	if ((Type.compare l compareWith) &&
		(Type.compare r compareWith)) then result
	else (raise ErrorInvalidTypeEq)

let type_of (expression: expression) (_in: program): system_type =
	type_of_expression _in (Environment([])) expression


