
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
		(typ: system_type) =
	match typ with
	| PrimitiveType value -> string_of_type value
	| UserDefinedType value -> value
	| NullType -> "Null"
