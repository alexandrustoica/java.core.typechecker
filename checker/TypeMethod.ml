

exception IncompatibleReturnTypeFromMethodExpression

let type_of meth context =
	match meth with
	| Method.MethodDeclaration (typ, name, params, expr) ->
		let records = params |> List.map Record.record_of_parameter in
		let new_context = records |> Context.insert_all context in 
		let expr_type = TypeExpression.type_of expr new_context in
		match (Type.compare expr_type typ) with
		| false -> raise IncompatibleReturnTypeFromMethodExpression
		| true -> typ
		
