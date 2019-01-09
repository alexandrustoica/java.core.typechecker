

let type_of program =
	let context = Context.Context(program, Environment.Environment([]))
	and classes = (Program.classes_of program) in
	let type_of_class = fun it -> TypeClass.type_of it context in
	let type_classes = classes |> List.map type_of_class in ()