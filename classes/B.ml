open Type
open Class
open Field
open Method
open Parameter
open Expression

let lastAssignment = Assign(VarWithField("this", "f2"), Var(VarWithName("z")))
let last = Compose(lastAssignment, Var(VarWithName("z")))

let thenExpr = Assign(VarWithName("z"), New("A", [VarWithName("m")]))
let elseExpr = Assign(VarWithName("z"), New("A", [VarWithName("n")]))
let ifExpr = If(VarWithName("m"), thenExpr, elseExpr)

let mAssign = Assign(VarWithName("m"), Operation(CompareOperation(GT(
					Operation(IntOperation(IntMinus(
								Var(VarWithField("x", "f1")), Var(VarWithField("y", "f1"))))),
					Var(VarWithName("n"))))))

let first = Assign(VarWithName("n"),
		Operation(IntOperation(IntMinus(
					Call(VarWithName("x"), "m1", [KInt(1); KInt(2)]),
					Call(VarWithName("y"), "m2", [KInt(2); KInt(1)])))))
let localNestedBlock = LocalVar(PrimitiveType(CoreBool), VarWithName("m"), Compose(mAssign, ifExpr))
let expression = Compose(first, Compose(localNestedBlock, last))
let block = LocalVar(PrimitiveType(CoreInt), VarWithName("n"), expression)
let funBlock = LocalVar(UserDefinedType("A"), VarWithName("z"), block)

let params = [
	Parameter(UserDefinedType("A"), "x");
	Parameter(UserDefinedType("A"), "y")
]

let methods = [MethodDeclaration(UserDefinedType("A"), "m2", params, funBlock)]
let fields = [FieldDeclaration(UserDefinedType("A"), "f2")]

let cls = InheritanceDeclaration("B", "A", fields, methods)