
type environment = Environment of Record.record list

let records_of = function Environment values -> values

let string_of = function
	| Environment values -> values
			|> List.map Record.string_of
			|> List.fold_left (fun acc it -> acc ^ " " ^ it) ""

let find_in environment name =
	let equal_by_name = fun it -> (Record.name_of it) = name in
	match environment with
	| Environment values -> values |> List.find equal_by_name |> Record.type_of

let insert_in environment record =
	match environment with
	| Environment values -> Environment(record :: values)
