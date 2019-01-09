exception DuplicatedDefinitionOfFieldInClass
exception DuplicatedDefinitionOfMethodInClass
exception DuplicatedElementFoundInClass
exception NoEntryClassFoundInProgram
exception NoEntryPointFunctionFoundInProgram

let detect_duplicates = function
	| Program.Program classes ->
			let find = fun it -> Class.duplications it in
			let check = function
				| ([], []) -> ()
				| (_, []) -> raise DuplicatedDefinitionOfFieldInClass
				| ([], _) -> raise DuplicatedDefinitionOfMethodInClass
				| _ -> raise DuplicatedElementFoundInClass
				| _ -> raise DuplicatedElementFoundInClass in
			classes |> List.map find |> List.iter check

let detect_entry_point in_program =
	let main_type = Type.UserDefinedType("Main") in
	let main_class = Program.find_class main_type in_program in
	match main_class with
	| None -> raise NoEntryClassFoundInProgram
	| Some cls ->
			match (Class.find_method_by "main" cls) with
			| None -> raise NoEntryPointFunctionFoundInProgram
			| Some meth -> ()

let type_of program =
	let context = Context.Context(program, Environment.Environment([]))
	and classes = (Program.classes_of program) in
	let type_of_class = fun it -> TypeClass.type_of it context in
	let _ = classes |> List.map type_of_class in
	let _ = detect_duplicates program in
	let _ = detect_entry_point program in ()