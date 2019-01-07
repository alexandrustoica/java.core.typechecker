open Type

type variable =
	| KNull
	| KInt of int
	| KFloat of float
	| KBool of bool
	| VarWithName of string
	| VarWithField of string * string

type expression =
	| Void
	| Var of variable
	| Assign of variable * expression
	| LocalVar of system_type * variable * expression
	| Compose of expression * expression
	| If of variable * expression * expression
	| Operation of operation
	| New of string * variable list
	| Call of variable * string * variable list
	| While of variable * expression
	| Cast of string * variable
	| InstanceOf of variable * string

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

val string_of_expression: expression -> string
val string_of_operation: operation -> string
val string_of_variable: variable -> string
