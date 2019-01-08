
type record = Record of Type.system_type * string

let string_of = function
	| Record (typ, name) -> (Type.string_of_type typ) ^ " -- " ^ name

let type_of = function Record (typ, _) -> typ
let name_of = function Record (_, name) -> name

let record_of typ variable =
	match variable with
	| Variable.VarWithName name -> Record(typ, name)
	| Variable.VarWithField (name, field) -> Record(typ, name)
	| it -> Record(typ, (Variable.string_of it))
