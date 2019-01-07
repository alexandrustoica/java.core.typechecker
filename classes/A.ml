open Type
open Class
open Field
open Method 
open Parameter
open Expression


let expr1 = Assign(VarWithName("c"), 
	Operation(IntOperation(IntPlus(
	Var(VarWithName("a")), Var(VarWithName("b"))))))

let expr2 = Assign(VarWithField("this", "f1"), 
	Operation(IntOperation(IntPlus(
		Var(VarWithField("this", "f1")), Var(VarWithName("c"))))))

let expr3 = Var(VarWithName("c"))

let expression = Compose(expr1, Compose(expr2, expr3))

let function_body = LocalVar(PrimitiveType(CoreInt), VarWithName("c"), expression)

let parameters = [
	Parameter(PrimitiveType(CoreInt), "a"); 
	Parameter(PrimitiveType(CoreInt), "b")
]
	
let methods =
	[MethodDeclaration(PrimitiveType(CoreInt), "m1", parameters, function_body)]

let fields = 
	[FieldDeclaration(PrimitiveType(CoreInt), "f1")]

let cls: class_declaration =
	ClassDeclaration("A", fields, methods)
