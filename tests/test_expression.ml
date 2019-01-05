open Expression
open OUnit


(* Test Fixture *)
let test_to_string = "To String Test" >:::
[
  "Void" >:: (fun () -> 
    assert_equal (Expression.to_string Expression.Void) "";
  );
]

let _ = run_test_tt test_to_string

