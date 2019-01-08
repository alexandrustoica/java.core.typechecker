
type expression =
	| Void
	| Var of Variable.variable
	| Assign of Variable.variable * expression
	| LocalVar of Type.system_type * Variable.variable * expression
	| Compose of expression * expression
	| If of Variable.variable * expression * expression
	| Operation of operation
	| New of string * Variable.variable list
	| Call of Variable.variable * string * Variable.variable list
	| While of Variable.variable * expression
	| Cast of string * Variable.variable
	| InstanceOf of Variable.variable * string

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
	
