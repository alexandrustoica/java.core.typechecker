
type expression =
  | Plus of expression * expression
  | Minus of expression * expression
  | Divide of expression * expression
  | Times of expresssion * expression
  | Value of string;;

let () = 
  Times(Value "2", Plus(Value "x", Value "y"));;
