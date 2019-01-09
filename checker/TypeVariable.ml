open Type
open Variable

let type_of variable context =
	let environment = Context.environment_of context 
	and program = Context.program_of context in
	let search with_name = Environment.find_in environment with_name 
	and fields_of class_name = ClassFields.fields_in program class_name in
	match variable with
	| KNull -> NullType
	| KInt _ -> PrimitiveType(CoreInt)
	| KFloat _ -> PrimitiveType(CoreFloat)
	| KBool _ -> PrimitiveType(CoreBool)
	| VarWithName name -> search name
	| VarWithField (name, field) ->	
		let type_for_name = search name in
		let class_name = string_of_type type_for_name in
		let fields = fields_of class_name in
		fields |> Field.find_by name |> Field.type_of


let types_of vars context =
	vars |> List.map (fun it -> type_of it context)
