module Expression = struct
	
	type variable =
		| Name of string
		| Field of string * string
	
	
	type expression =
		| Null
		| Void
		| KInt of int
		| KFloat of float
		| KBool of bool
		| Var of variable
		| Assignment of variable * expression
		| Composition of expression * expression
		| If of variable * expression * expression

	let string_of_variable (variable: variable) =
		 match variable with
		| Name value -> value
		| Field (variable, field) -> variable ^ "." ^ field
	
	let rec string_of_expression (expression: expression) = 
		match expression with
		| Null -> "Null"
		| Void -> ""
		| Var variable -> string_of_variable variable
		| Assignment (variable, expr) -> 
			(string_of_variable variable) ^ " = " ^ (string_of_expression expr)
		| KInt value -> string_of_int value
		| KFloat value -> string_of_float value
		| KBool value -> string_of_bool value
		| Composition (left, right) -> 
			(string_of_expression left) ^ ";" ^ (string_of_expression right)
		| If (condition, t, f) -> 
			"if(" ^ (string_of_variable condition) ^ ")" ^
			" then " ^ (string_of_expression t) ^
			" else " ^ (string_of_expression f)
	
end