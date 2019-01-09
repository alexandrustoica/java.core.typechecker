open Expression
open Variable

let program = (Program.Program([A.cls; B.cls; M.cls]))

let _ =	
	let expression = LocalVar(UserDefinedType("A"), VarWithName("a"),
		Call(VarWithName("a"), "m1", [KInt(4); KInt(4)])) in
	let meth = Method.MethodDeclaration(PrimitiveType(CoreInt), "m2", [], expression)
	and context = Context.Context(program, Environment.Environment([])) in
	context |> TypeMethod.type_of meth |> Type.string_of_type |> print_endline

let _ = let relations = Relation.extend (Relation.relations_in program) in
	relations |> List.map Relation.string_of_relation
	|> List.fold_left (fun acc it -> acc ^ "\n" ^ it) ""

let _ = Program.user_types_in program
	|> Type.string_of_types
	|> print_endline

let _ =
	let right = Type.UserDefinedType("B")
	and left = Type.UserDefinedType("Object") in
	let result = RelatedType.is_related right left program in
	string_of_bool result |> print_endline

let _ = print_endline (Program.string_of program)
