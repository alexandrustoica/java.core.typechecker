
open Type
open Record

type environment = Environment of record list


let records_of (env: environment): record list =
	match env with
	| Environment values -> values

let string_of (env: environment): string =
	List.fold_left (fun acc it -> acc ^ " " ^ it) "" 
	(List.map (fun it -> Record.string_of it) (records_of env))

let find_in (env: environment) (name: string): system_type =
	match env with
	| Environment values -> type_of (List.find (fun it -> (name_of it) = name) values)

let insert_in (env: environment) (record: record): environment =
	match env with
	| Environment values -> Environment([record] @ values)

