open Typ.Type

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
		| LocalVar of system_type * variable * expression
		| Composition of expression * expression
		| If of variable * expression * expression
		| Operation of operation
		| New of string * variable list
		| Call of variable * string * variable list
		| While of variable * expression
		
	and operation =
		| IntOperation of int_operation
		| FloatOperation of float_operation
		| BoolOperation of bool_operation
		| CompareOperation of compare_operation
	
	and int_operation =
		| IntPlus of expression * expression
		| IntMinus of expression * expression
		| IntDivide of expression * expression
		| IntTimes of expression * expression
	
	and float_operation =
		| FloatPlus of expression * expression
		| FloatMinus of expression * expression
		| FloatDivide of expression * expression
		| FloatTimes of expression * expression
		
	and bool_operation =
		| And of expression * expression
		| Or of expression * expression
		| Not of expression
		
	and compare_operation =
		| LT of expression * expression
		| GT of expression * expression
		| LE of expression * expression
		| GE of expression * expression
		| EQ of expression * expression
		| NE of expression * expression


	let string_of_variable (variable: variable) =
		 match variable with
		| Name value -> value
		| Field (variable, field) -> variable ^ "." ^ field
	
	let rec string_of_expression (expression: expression) = 
		match expression with
		| Null -> "Null"
		| Void -> ""
		| Var variable -> string_of_variable variable
		| LocalVar (typ, var, scope) -> 
			"{(" ^ (string_of_type typ) ^ " " ^ (string_of_variable var) ^
			 ")" ^ (string_of_expression scope) ^ "}"
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
		| Operation op -> string_of_operation op
		| New (name, vars) -> 
			"new " ^ name ^
			"(" ^ List.fold_left (fun acc it -> it ^ acc) "" 
			(List.map (fun it -> (string_of_variable it) ^ " ") vars) ^ ")"
		| Call (var, name, args) ->
			(string_of_variable var) ^ "." ^ name 
			^ "(" ^ (List.fold_left (fun acc it -> acc ^ " " ^ (string_of_variable it)) "" args) ^ ")"
		| While (var, expr) -> "whle (" ^ (string_of_variable var) ^ ")" ^ 
		"{" ^ (string_of_expression expr) ^ "}"
	
	and string_of_int_operation (op) =
		match op with
		| IntPlus (l, r) -> (string_of_expression l) ^ " + " ^ (string_of_expression r)
		| IntMinus (l, r) -> (string_of_expression l) ^ " - " ^ (string_of_expression r)
		| IntDivide (l, r) -> (string_of_expression l) ^ " / " ^ (string_of_expression r)
		| IntTimes (l, r) -> (string_of_expression l) ^ " * " ^ (string_of_expression r)
	
	
	and string_of_float_operation (op) =
		match op with
		| FloatPlus (l, r) -> (string_of_expression l) ^ " .+ " ^ (string_of_expression r)
		| FloatMinus (l, r) -> (string_of_expression l) ^ " .- " ^ (string_of_expression r)
		| FloatDivide (l, r) -> (string_of_expression l) ^ " ./ " ^ (string_of_expression r)
		| FloatTimes (l, r) -> (string_of_expression l) ^ " .* " ^ (string_of_expression r)
	
	and string_of_bool_operation (op) =
		match op with
		| And (l, r) -> (string_of_expression l) ^ " && " ^ (string_of_expression r)
		| Or (l, r) -> (string_of_expression l) ^ " || " ^ (string_of_expression r)
		| Not e ->  "!" ^ (string_of_expression e)
	
	and string_of_compare_operation (op) =
		match op with
		| LT (l, r) -> (string_of_expression l) ^ " < " ^ (string_of_expression r)
		| GT (l, r) -> (string_of_expression l) ^ " > " ^ (string_of_expression r)
		| LE (l, r) -> (string_of_expression l) ^ " <= " ^ (string_of_expression r)
		| GE (l, r) -> (string_of_expression l) ^ " >= " ^ (string_of_expression r)
		| EQ (l, r) -> (string_of_expression l) ^ " == " ^ (string_of_expression r)
		| NE (l, r) -> (string_of_expression l) ^ " != " ^ (string_of_expression r)
	
	and string_of_operation (op: operation) =
		match op with
		| IntOperation operation -> string_of_int_operation operation
		| FloatOperation operation -> string_of_float_operation operation
		| BoolOperation operation -> string_of_bool_operation operation
		| CompareOperation operation -> string_of_compare_operation operation
	

end