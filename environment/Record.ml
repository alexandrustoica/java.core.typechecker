open Type
open Field
open Expression

type record = Record of system_type * string

let string_of (record: record): string =
	match record with
	| Record (typ, name) -> (string_of_type typ) ^ " -- " ^ name

let type_of (record: record): system_type =
	match record with
	| Record (typ, _) -> typ

let name_of (record: record): string =
	match record with
	| Record (_, name) -> name

let record_of
		(typ: system_type)
		(variable: variable): record =
	match variable with
	| VarWithName name -> Record(typ, name)
	| _ -> Record(typ, "CheckThisPoint") (*REFACTOR*)
