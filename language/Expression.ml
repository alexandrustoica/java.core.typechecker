open Type
include ExpressionTypes

let rec string_of_expression (expression: expression) =
	match expression with
	| Void -> ""
	| Var variable -> Variable.string_of variable
	| LocalVar (typ, var, scope) ->
			"{ (" ^ (string_of_type typ) ^ " " ^ (Variable.string_of var) ^
			") " ^ (string_of_expression scope) ^ " }"
	| Assign (variable, expr) ->
			(Variable.string_of variable) ^ " = " ^ (string_of_expression expr)
	| Compose (left, right) ->
			(string_of_expression left) ^ " ; " ^ (string_of_expression right)
	| If (condition, t, f) ->
			"if(" ^ (Variable.string_of condition) ^ ")" ^
			" then " ^ (string_of_expression t) ^
			" else " ^ (string_of_expression f)
	| Operation op -> string_of_operation op
	| New (name, []) -> "new " ^ name ^ "()"
	| New (name, h:: t) ->
			"new " ^ name ^
			"(" ^ List.fold_left (fun acc it -> acc ^ "," ^ (Variable.string_of it))
				(Variable.string_of h) t ^ ")"
	| Call (var, name, []) -> (Variable.string_of var) ^ "." ^ name ^ "()"
	| Call (var, name, h:: t) ->
			(Variable.string_of var) ^ "." ^ name
			^ "(" ^ (List.fold_left
					(fun acc it -> acc ^ "," ^ (Variable.string_of it))
					(Variable.string_of h) t) ^ ")"
	| While (var, expr) -> "whle (" ^ (Variable.string_of var) ^ ")" ^
			" { " ^ (string_of_expression expr) ^ " } "
	| Cast (cls, var) -> "(" ^ cls ^ ") " ^ (Variable.string_of var)
	| InstanceOf (var, cls) -> (Variable.string_of var) ^ " instanceOf " ^ cls

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
	| Not e -> "!" ^ (string_of_expression e)

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