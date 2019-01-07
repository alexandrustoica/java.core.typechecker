open Type
open Class
open Field
open Method
open Parameter
open Expression


let assignmentB = Assign(VarWithName("o1"), New("B", [KInt(0); KNull]))
let assignmentA = Assign(VarWithName("o2"), New("A", [KInt(2)]))
let assignmentC = Assign(VarWithName("o3"), New("A", [KInt(3)]))

let call = Call(VarWithName("o1"), "m2", [VarWithName("o2"); VarWithName("m3")])

let last = Assign(VarWithName("o2"), call)
let third = LocalVar(UserDefinedType("A"), VarWithName("o3"), Compose(assignmentC, last))
let second = LocalVar(UserDefinedType("A"), VarWithName("o2"), Compose(assignmentA, third))
let first = LocalVar(UserDefinedType("B"), VarWithName("o1"), Compose(assignmentB, second))

let meth = MethodDeclaration(PrimitiveType(CoreUnit), "main", [], first)

let cls = ClassDeclaration("Main", [], [meth])
