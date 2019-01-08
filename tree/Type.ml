
type typ = CoreInt | CoreBool | CoreUnit | CoreFloat

type system_type =
	| PrimitiveType of typ
	| UserDefinedType of string
	| NullType

let compare
		(left: typ)
		(right: typ): bool =
	match (left, right) with
	| (CoreInt, CoreInt) -> true
	| (CoreBool, CoreBool) -> true
	| (CoreUnit, CoreUnit) -> true
	| (CoreFloat, CoreFloat) -> true
	| (_, _) -> false

let compare
		(left: system_type)
		(right: system_type): bool =
	match (left, right) with
	| (UserDefinedType l, UserDefinedType r) -> l = r
	| (PrimitiveType l, PrimitiveType r) -> compare l r
	| (NullType, NullType) -> true
	| (_, _) -> false

let string_of_type
		(typ: typ) : string =
	match typ with
	| CoreInt -> "Int"
	| CoreBool -> "Bool"
	| CoreUnit -> "Unit"
	| CoreFloat -> "Float"

let string_of_type
		(typ: system_type): string =
	match typ with
	| PrimitiveType value -> string_of_type value
	| UserDefinedType value -> value
	| NullType -> "Null"

let rec string_of_types
	(types: system_type list): string =
		match types with
		| [] -> ""
		| [last] -> (string_of_type last)
		| h::t -> (string_of_type h) ^ " ; " ^ string_of_types t

let is_user_defined (typ: system_type): bool =
	match typ with
	| UserDefinedType _ -> true
	| _ -> false

let is_user_defined
	(types: system_type list): bool = types 
		|> List.map (fun it -> is_user_defined it) 
		|> List.fold_left (fun acc it -> acc && it) true
		
