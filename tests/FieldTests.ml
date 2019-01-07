open OUnit2
open Field
open SystemType

let field = FieldDeclaration(UserDefinedType("A"), "Test")

let tests = "given a field description (A Test)" >::: [
  
	"expect field's type eq to A" >:: (fun _ ->
		assert_equal
			(SystemType.Type.compare 
			(type_of_field field) 
			(UserDefinedType("A"))) true);
			
  "expect field's name eq to Test" >:: (fun _ ->
		assert_equal 
			(name_of_field field) "Test");

]

let _ = run_test_tt_main tests