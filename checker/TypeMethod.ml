

exception IncompatibleReturnTypeFromMethodExpression

let override_method_of meth in_class = 
	Class.methods_of_class in_class 
	|> List.find_opt (fun x -> (Method.name_of x) == (Method.name_of meth)) 

let is_overriding left right context = 
	(RelatedType.are_connected 
		(Method.parameters_of left |> List.map Parameter.type_of)
		(Method.parameters_of right |> List.map Parameter.type_of)
		(Context.program_of context)) &&
	(RelatedType.is_related 
		(Method.type_of left) 
		(Method.type_of right)
		(Context.program_of context)) && 
		(Method.name_of left) == (Method.name_of right)

let type_of meth context =
	match meth with
	| Method.MethodDeclaration (typ, name, params, expr) ->
		let records = params |> List.map Record.record_of_parameter in
		let new_context = records |> Context.insert_all context in 
		let expr_type = TypeExpression.type_of expr new_context in
		match (Type.compare expr_type typ) with
		| false -> raise IncompatibleReturnTypeFromMethodExpression
		| true -> typ
		
		