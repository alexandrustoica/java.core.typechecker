open Expression.Expression

let () =
	let expression = Call(Field("Object", "test"), "test", [Name("test")])
	in print_endline (string_of_expression expression)
	
	
	(* let expression = Expression.Null                                            *)
	(* in let parameter = Parameter(PrimitiveType(CoreUnit), "test")               *)
	(* in let methodDeclaration = MethodDeclaration(                               *)
	(* 	PrimitiveType(CoreInt), "function", [parameter], expression)              *)
	(* in let fieldDeclaration = FieldDeclaration (PrimitiveType(CoreInt), "test") *)
	(* in let classDeclaration = ClassDeclaration("Main", "Object",                *)
	(* FieldDeclarations([fieldDeclaration]),                                      *)
	(* MethodDeclarations([methodDeclaration]))                                    *)
	(* in print_endline                                                            *)
	(* 	(to_string (Program(ClassDeclarations([classDeclaration]))))              *)