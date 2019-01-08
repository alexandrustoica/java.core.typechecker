
open Type
open Record

type environment = Environment of Record.record list

let records_of = function
	| Environment values -> values

let string_of = function
	| Environment values -> values
			|> List.map Record.string_of
			|> List.fold_left (fun acc it -> acc ^ " " ^ it) ""

let find_in (env: environment) (name: string): Type.system_type =
	match env with
	| Environment values ->
			values |> List.find (fun it -> (Record.name_of it) = name) |> type_of

let insert_in (env: environment) (record: record): environment =
	match env with
	| Environment values -> Environment(record :: values)

