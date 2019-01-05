module Type = struct


	type typ = CoreInt | CoreBool | CoreUnit | CoreFloat 
	
	type system_type = 
		| PrimitiveType of typ
		| UserDefinedType of string
		| NullType
	
	let string_of_type (typ: typ) = match typ with
	| CoreInt -> "Int"
	| CoreBool -> "Bool"
	| CoreUnit -> "Unit"
	| CoreFloat -> "Float"
	
	let string_of_type (typ: system_type) = match typ with
	| PrimitiveType value -> string_of_type value
	| UserDefinedType value -> value
	| NullType -> "Null"

end