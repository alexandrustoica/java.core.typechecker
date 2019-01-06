
open SystemType.Type
open Expression.Expression

module Environment =
struct
	
	type record = Record of (system_type * string)

	type environment = Environment of record list
			
	let records_of (env: environment): record list =
		match env with
		| Environment values -> values		
	
	let type_of (record: record): system_type =
		match record with
		| Record (typ, _) -> typ

	let name_of (record: record): string =
		match record with
		| Record (_, name) -> name
	
	let string_of (record: record): string =
		match record with
		| Record (typ, name) -> (string_of_type typ) ^ " -- " ^ name

	let string_of (env: environment): string =
		match env with
		| Environment values -> List.fold_left 
		(fun acc it -> (string_of it) ^ "\n" ^ acc) "" values
	
	
	let find_in (env: environment) (name: string): system_type =
		match env with
		| Environment values -> type_of (List.find (fun it -> (name_of it) = name) values)
	
	let insert_in (env: environment) (record: record): environment =
		match env with
		| Environment values -> Environment([record] @ values)
	
	let to_record
			(typ: system_type)
			(variable: variable): record =
		match variable with
		| VarWithName name -> Record(typ, name)
		| _ -> Record(typ, "CheckThisPoint") (*REFACTOR*)

end