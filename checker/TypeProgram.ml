exception DuplicatedDefinitionOfFieldInClass
exception DuplicatedDefinitionOfMethodInClass
exception DuplicatedElementFoundInClass
exception NoEntryClassFoundInProgram
exception NoEntryPointFunctionFoundInProgram
exception DuplicatedClassDeclarationDetected
exception InheritanceCycleWithinClassDeclarations

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

let detect_duplicated_classes in_program =
	match (Program.duplications in_program) with
	| [] -> ()
	| _ -> raise DuplicatedClassDeclarationDetected

let transitive_set in_program =
	Relation.extend (Relation.relations_in in_program)

let verify_transitivity in_program =
	let xs = transitive_set in_program in
	let result = xs |> List.filter Relation.is_identity in
	match result with
	| [] -> true
	| _ -> raise InheritanceCycleWithinClassDeclarations

let type_of program =
	let context = Context.Context(program, Environment.Environment([]))
	and classes = (Program.classes_of program) in
	let type_of_class = fun it -> TypeClass.type_of it context in
	let _ = classes |> List.map type_of_class in
	let _ = detect_duplicates program in
	let _ = detect_entry_point program in
	let _ = detect_duplicated_classes program in
	let _ = verify_transitivity program in ()