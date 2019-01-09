
let type_of cls context =
	let methods = Class.methods_of_class cls
	and record = Record.Record(Type.UserDefinedType(
		Class.name_of_class cls), "this") in
	let new_context = Context.insert_in context record in
	let type_of_method = fun it -> TypeMethod.type_of it new_context in
	let _ = methods |> List.map type_of_method in ()