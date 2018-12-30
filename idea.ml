type expression =
  | Plus of expression * expression
  | Minus of expression * expression
  | Divide of expression * expression
  | Times of expression * expression
  | Value of string;;

let rec to_string expr =
  match expr with
  | Plus (left, right) -> "(" ^ to_string left ^ " + " ^ to_string right ^ ")"
  | Minus (left, right) -> "(" ^ to_string left ^ " - " ^ to_string right ^ ")"
  | Times (left, right) -> "(" ^ to_string left ^ " * " ^ to_string right ^ ")"
  | Divide (left, right) -> "(" ^ to_string left ^ " / " ^ to_string right ^ ")"
  | Value value -> value;;


let () = 
  print_endline (to_string (Times (Value "2", Plus(Value "x", Value "y"))));;
