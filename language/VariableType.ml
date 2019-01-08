
type variable =
	| KNull
	| KInt of int
	| KFloat of float
	| KBool of bool
	| VarWithName of string
	| VarWithField of string * string
