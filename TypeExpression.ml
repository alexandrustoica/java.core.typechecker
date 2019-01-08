open Expression
open Type
open Program
open ClassFields
open Field
open Environment
open Record 
open TypeVariable
open Context



exception ErrorInvalidTypeEq
exception ErrorInvalidNewType
exception UnableToCastType
exception UnrelatedTypeException
exception UnableToCastVariableToBool
exception UnableToCompareUserDefinedTypes



exception UnrelatedTypesInAssignment of string



let is_user_defined (typ: string) (_in: program): bool =
		List.exists (fun it -> (string_of_type it) = typ) (Program.user_types_in _in)




let rec type_of 
	(expression: expression)
	(context: context): system_type =
	let _ = (print_endline (Environment.string_of (env_of context)))
	in let type_of_variable var = TypeVariable.type_of var context
	in match expression with
	| Void -> PrimitiveType(CoreUnit)
	| Var var -> type_of_variable var
	| LocalVar (typ, var, expr) ->
			let new_record = record_of typ var
			in let new_context = new_record |> (insert_in context)
			in type_of expr new_context
	| Assign (var, expr) ->
			let variable_type = TypeVariable.type_of var context
			in let expression_type = type_of expr context
			in let related = RelatedType.is_related variable_type expression_type (program_of context)
			in let error = (string_of_type variable_type) ^ " is not related with " ^ (string_of_type expression_type) 
			in if related then PrimitiveType(CoreUnit) 
			else (raise (UnrelatedTypesInAssignment error))
	| Compose (left, right) -> let _ = (type_of left context) in (type_of right context)
	| Operation operation -> type_of_operation operation context
	
	
	
	| If (var, t, f) -> 
		let _ = (type_of t context)
		in (type_of f context) (* Find Commune Parent Between l r *)

	| New (typ, args) -> 
					(* Check Fields' || Args Types *)
		let result = UserDefinedType(typ) in
		if (is_user_defined typ (program_of context)) 
		then result else (raise ErrorInvalidNewType) 
	
	
	
	| Cast (typ_as_string, var) ->
		if ((is_user_defined typ_as_string (program_of context)) && 
		(RelatedType.is_connected (UserDefinedType(typ_as_string)) 
		(TypeVariable.type_of var context) (program_of context))) then
			UserDefinedType(typ_as_string) else (raise UnableToCastType)
			
			
	| InstanceOf (var, typ_as_string) -> 
		if((is_user_defined typ_as_string (program_of context)) &&
		(RelatedType.is_connected (UserDefinedType(typ_as_string)) 
		(TypeVariable.type_of var context) (program_of context))) then
			(PrimitiveType(CoreBool)) else (raise UnrelatedTypeException) 
			
			
	| While (var, expr) -> 
		if (Type.compare (TypeVariable.type_of var context) (PrimitiveType(CoreBool))) then
			let result = (type_of expr context) in (PrimitiveType(CoreBool)) else
				(raise UnableToCastVariableToBool)
	
	
	| _ -> PrimitiveType(CoreInt)
	


and type_of_operation
		(operation: operation)
		(context: context): system_type =
	match operation with
	| IntOperation op -> type_of_int_operation op context
	| FloatOperation op -> type_of_float_operation op context
	| BoolOperation op -> type_of_bool_operation op context
	| CompareOperation op -> type_of_compare_operation op context

and type_of_int_operation
		(operation: int_operation)
		(context: context): system_type =
	let type_of expr= type_of expr context
	in let type_result = PrimitiveType(CoreInt)
	in let (==) = validate_types_eq type_result type_result 
	in match operation with
	| IntPlus (l, r) -> (type_of l) == (type_of r)
	| IntMinus (l, r) -> (type_of l) == (type_of r)
	| IntDivide (l, r) -> (type_of l) == (type_of r) 
	| IntTimes (l, r) -> (type_of l) == (type_of r)

and type_of_float_operation
		(operation: float_operation)
		(context: context): system_type =
	let type_of expr = type_of expr context
	in let type_result = PrimitiveType(CoreFloat)
	in let (==) = validate_types_eq type_result type_result 
	in match operation with
	| FloatPlus (l, r) -> (type_of l) == (type_of r)
	| FloatMinus (l, r) -> (type_of l) == (type_of r)
	| FloatDivide (l, r) -> (type_of l) == (type_of r)
	| FloatTimes (l, r) -> (type_of l) == (type_of r)

and type_of_bool_operation
	(operation: bool_operation)
	(context: context): system_type =
	let type_of expr = type_of expr context
	in let type_result = PrimitiveType(CoreBool)
	in let (==) = validate_types_eq type_result type_result 
	in match operation with
	| And (l, r) -> (type_of l) == (type_of r)
	| Or (l, r) -> (type_of l) == (type_of r)
	| Not expr -> (type_of expr) == (type_result)


(** TODO Check if user defined type. *)
and type_of_compare_operation
	(operation: compare_operation)
	(context: context): system_type =
	let type_of expr = type_of expr context
	in let check a b = (not ((is_user_defined (Type.string_of_type a) (program_of context)) && 
		(is_user_defined (Type.string_of_type b) (program_of context))))
	in let type_result = PrimitiveType(CoreBool)
	in let validate (cmp: system_type) = validate_types_eq cmp type_result 
	in let check_exists a b c = (if (check a c) then (validate a b c) 
			else (raise UnableToCompareUserDefinedTypes))
	in match operation with
	| LT (l, r) -> let type_of_l = (type_of l) in check_exists type_of_l type_of_l (type_of r)
	| GT (l, r) -> let type_of_l = (type_of l) in check_exists type_of_l type_of_l (type_of r)
	| LE (l, r) -> let type_of_l = (type_of l) in check_exists type_of_l type_of_l (type_of r)
	| GE (l, r) -> let type_of_l = (type_of l) in check_exists type_of_l type_of_l (type_of r)
	| EQ (l, r) -> let type_of_l = (type_of l) in check_exists type_of_l type_of_l (type_of r)
	| NE (l, r) -> let type_of_l = (type_of l) in check_exists type_of_l type_of_l (type_of r)

and validate_types_eq
		(compareWith: system_type)
		(result: system_type)
		(l: system_type)
		(r: system_type): system_type =
	if ((Type.compare l compareWith) &&
		(Type.compare r compareWith)) then result
	else (raise ErrorInvalidTypeEq)

