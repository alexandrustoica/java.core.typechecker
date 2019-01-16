
type typ = CoreInt | CoreBool | CoreUnit | CoreFloat

type system_type =
	| PrimitiveType of typ
	| UserDefinedType of string
	| NullType

let compare left right =
	match (left, right) with
	| (CoreInt, CoreInt) -> true
	| (CoreBool, CoreBool) -> true
	| (CoreUnit, CoreUnit) -> true
	| (CoreFloat, CoreFloat) -> true
	| (_, _) -> false

let compare left right =
	match (left, right) with
	| (NullType, UserDefinedType _) -> true
	| (UserDefinedType _, NullType) -> true
	| (UserDefinedType l, UserDefinedType r) -> l = r
	| (PrimitiveType l, PrimitiveType r) -> compare l r
	| (NullType, NullType) -> true
	| (_, _) -> false

let string_of_type = function
	| CoreInt -> "Int"
	| CoreBool -> "Bool"
	| CoreUnit -> "Unit"
	| CoreFloat -> "Float"

let string_of_type = function
	| PrimitiveType value -> string_of_type value
	| UserDefinedType value -> value
	| NullType -> "Null"

let rec string_of_types = function
	| [] -> ""
	| [last] -> (string_of_type last)
	| h::t -> (string_of_type h) ^ " ; " ^ string_of_types t

let is_user_defined = function
	| UserDefinedType _ -> true
	| _ -> false

let rec are_equal = function
	| [] -> true
	| [x; y] -> compare x y
	| x::y::t -> (compare x y) && (are_equal t) 

let is_user_defined types =
	types
	|> List.map (fun it -> is_user_defined it)
	|> List.fold_left (fun acc it -> acc && it) true
