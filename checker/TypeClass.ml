
exception InvalidMethodOverridingWithinClass

let check_overrides_of meth in_class context =
	let override = TypeMethod.override_method_of meth in_class in
	match override with
	| None -> true
	| Some x -> 
		let result = (TypeMethod.is_overriding meth x context) in
		match result with
		| false -> raise InvalidMethodOverridingWithinClass
		| true -> true

let type_of cls context =
	let methods = Class.methods_of_class cls
	and record = Record.Record(Type.UserDefinedType(
		Class.name_of_class cls), "this") in
	let new_context = Context.insert_in context record in
	let type_of_method = fun it -> TypeMethod.type_of it new_context in
	let _ = methods |> List.map type_of_method in 
	let _ = methods |> List.map 
		(fun x -> check_overrides_of x cls new_context) in ()