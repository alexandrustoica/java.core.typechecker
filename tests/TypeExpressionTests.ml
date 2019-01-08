open OUnit2
open TypeExpression
open Context
open Program
open Expression

let context = Context(Program([A.cls; B.cls; M.cls]), Environment([]))
let tests = "Type of expression tests" >::: [
  
	"Void Expression Test" >:: (fun _ ->
		assert_equal (type_of Void context) (PrimitiveType(CoreUnit)));
			
  "Variable Type Test" >:: (fun _ ->
		assert_equal (type_of (Var(KInt(4))) context) (PrimitiveType(CoreInt)));

	"Local Variable Test" >:: (fun _ ->
		let expr = LocalVar(UserDefinedType("A"), VarWithName("a"), Var(VarWithName("a")))
		in assert_equal (type_of expr context) (UserDefinedType("A")));
	
	"Assignment Test" >:: (fun _ ->
		let expr = LocalVar(PrimitiveType(CoreInt), VarWithName("a"), Assign(VarWithName("a"), Var(KInt(4))))
		in assert_equal (type_of expr context) (PrimitiveType(CoreUnit)));
	
	"Compose Test" >:: (fun _ -> 
		let expr = Compose(Var(KInt(4)), Var(KFloat(3.0)))
		in assert_equal (type_of expr context) (PrimitiveType(CoreFloat)));
]

let _ = run_test_tt_main tests