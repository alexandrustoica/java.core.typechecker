open Typ
open Ast.AST

let () =
	
	let parameter = Parameter(PrimitiveType(CoreUnit), "test")
	in let methodDeclaration = MethodDeclaration(
		PrimitiveType(CoreInt), "function", [parameter], Expression("TEst"))
	in let fieldDeclaration = FieldDeclaration (PrimitiveType(CoreInt), "test")
	in let classDeclaration = ClassDeclaration("Main", "Object",
	FieldDeclarations([fieldDeclaration]), 
	MethodDeclarations([methodDeclaration]))
	
	in print_endline
		(to_string (Program(ClassDeclarations([classDeclaration]))))