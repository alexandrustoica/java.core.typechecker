open Type
include ExpressionTypes

let rec string_of_expression = function
	| Void -> " "
	| Var variable -> Variable.string_of variable
	| LocalVar (typ, var, scope) ->
			let system_type = Type.string_of_type typ
			and variable = Variable.string_of var
			and expression = string_of_expression scope in
			"{(" ^ system_type ^ " " ^ variable ^ ") " ^ expression ^ "}"
	| Assign (var, expr) ->
			(Variable.string_of var) ^ " = " ^ (string_of_expression expr)
	| Compose (left, right) ->
			(string_of_expression left) ^ " ; " ^ (string_of_expression right)
	| If (condition, t, f) ->
			let cond = Variable.string_of condition
			and _then = string_of_expression t
			and _else = string_of_expression f in
			"if(" ^ cond ^ ") \n then " ^ _then ^ "\n else " ^ _else
	| Operation operation -> string_of_operation operation
	| New (name, []) -> "new " ^ name ^ "()"
	| New (name, h :: t) ->
			let head = Variable.string_of h
			and compose = fun acc it -> acc ^ ", " ^ (Variable.string_of it) in
			let result = t |> List.fold_left compose head in
			"new " ^ name ^ "(" ^ result ^ ")"
	| Call (var, name, []) -> (Variable.string_of var) ^ "." ^ name ^ "()"
	| Call (var, name, h::t) ->
			let head = Variable.string_of h 
			and variable = Variable.string_of var
			and compose =  fun acc it -> acc ^ "," ^ (Variable.string_of it) in
			let result = t |> List.fold_left compose head in
			variable ^ "." ^ name ^ "(" ^ result ^ ")"
	| While (var, expr) -> "whle (" ^ (Variable.string_of var) ^ ")" ^
			" { " ^ (string_of_expression expr) ^ " } "
	| Cast (cls, var) -> "(" ^ cls ^ ") " ^ (Variable.string_of var)
	| InstanceOf (var, cls) -> (Variable.string_of var) ^ " instanceOf " ^ cls

and (>>=) left right = fun it ->
			(string_of_expression left) ^ it ^ (string_of_expression right)

and string_of_int_operation = function
	| IntPlus (left, right) -> (left >>= right) " + "
	| IntMinus (left, right) -> (left >>= right) " - "
	| IntDivide (left, right) -> (left >>= right) " / "
	| IntTimes (left, right) -> (left >>= right) " * "

and string_of_float_operation = function
	| FloatPlus (left, right) -> (left >>= right) " .+ "
	| FloatMinus (left, right) -> (left >>= right) " .- "
	| FloatDivide (left, right) -> (left >>= right) " ./ "
	| FloatTimes (left, right) -> (left >>= right) " .* "

and string_of_bool_operation = function
	| And (left, right) -> (left >>= right) " && "
	| Or (left, right) -> (left >>= right) " || "
	| Not expr -> "!" ^ (string_of_expression expr)

and string_of_compare_operation = function
	| LT (left, right) -> (left >>= right) " < "
	| GT (left, right) -> (left >>= right) " > "
	| LE (left, right) -> (left >>= right) " <= "
	| GE (left, right) -> (left >>= right) " >= "
	| EQ (left, right) -> (left >>= right) " == "
	| NE (left, right) -> (left >>= right) " != "

and string_of_operation = function
	| IntOperation operation -> string_of_int_operation operation
	| FloatOperation operation -> string_of_float_operation operation
	| BoolOperation operation -> string_of_bool_operation operation
	| CompareOperation operation -> string_of_compare_operation operation