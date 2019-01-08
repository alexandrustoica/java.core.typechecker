open Expression
open Variable

let program = (Program.Program([A.cls; B.cls; M.cls]))

let _ =	
	let expression = LocalVar(UserDefinedType("A"), VarWithName("a"), Var(KInt(3))) 
	and context = Context.Context(program, Environment.Environment([])) in
	context |> TypeExpression.type_of expression
	|> Type.string_of_type |> print_endline

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
