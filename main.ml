open Expression.Expression
open Ast.AST


let print_first_program =
	let first = Assign(VarWithName("c"), Operation(
		IntOperation(IntPlus(Var(VarWithName("a")), Var(VarWithName("b"))))))
	in let second = Assign(VarWithField("this", "f1"), Operation(
		IntOperation(IntPlus(Var(VarWithField("this", "f1")), Var(VarWithName("c"))))))
	in let third = Var(VarWithName("c"))
	in let expression = Compose(first, Compose(second, third))
	in let localBlock = LocalVar(PrimitiveType(CoreInt), VarWithName("c"), expression)
	in let parameters =
			[Parameter(PrimitiveType(CoreInt), "a"); Parameter(PrimitiveType(CoreInt), "b")]
	in let methods = [MethodDeclaration(PrimitiveType(CoreInt), "m1", parameters, localBlock)]
	in let fields = [FieldDeclaration(PrimitiveType(CoreInt), "f1")]
	in let classes = [ClassDeclaration("A", "Object", fields, methods)]		
	in print_endline (string_of_program (Program(classes)))


let print_second_program =
	let assignmentB = Assign(VarWithName("o1"), New("B", [KInt(0); KNull]))
	
	in let assignmentA = Assign(VarWithName("o2"), New("A", [KInt(2)]))
	in let assignmentC = Assign(VarWithName("o3"), New("A", [KInt(3)]))

	in let call = Call(VarWithName("o1"), "m2", [VarWithName("o2"); VarWithName("m3")]) 
	in let last = Assign(VarWithName("o2"), call)

	in let third = LocalVar(UserDefinedType("A"), VarWithName("o3"), Compose(assignmentC, last))
	in let second = LocalVar(UserDefinedType("A"), VarWithName("o2"), Compose(assignmentA, third))
	in let first = LocalVar(UserDefinedType("B"), VarWithName("o1"), Compose(assignmentB, second))

	in let meth = MethodDeclaration(PrimitiveType(CoreUnit), "main", [], first)
	in let cls = ClassDeclaration("Main", "Object", [], [meth])
	
	in print_endline (string_of_program (Program([cls])))


let () = print_second_program
