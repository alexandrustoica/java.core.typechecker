open Expression.Expression
open Ast.AST


let print_first_program =
	let first = Assignment(Name("c"), Operation(
		IntOperation(IntPlus(Var(Name("a")), Var(Name("b"))))))
	in let second = Assignment(Field("this", "f1"), Operation(
		IntOperation(IntPlus(Var(Field("this", "f1")), Var(Name("c"))))))
	in let third = Var(Name("c"))
	in let expression = Composition(first, Composition(second, third))
	in let localBlock = 
		LocalVar(PrimitiveType(CoreInt), Name("c"), expression)
	in let parameters =
			[Parameter(PrimitiveType(CoreInt), "a");
			Parameter(PrimitiveType(CoreInt), "b")]
	in let methods = 
				MethodDeclarations([
			MethodDeclaration(PrimitiveType(CoreInt), "m1", parameters, localBlock)
		])
	in let fields =
			FieldDeclarations([FieldDeclaration(PrimitiveType(CoreInt), "f1")])
	in let classes =
		ClassDeclarations([ClassDeclaration("A", "Object", fields, methods)])		
	in print_endline (string_of_program (Program(classes)))



let () = print_first_program
