
type listofints = int list

let hs: listofints = [1; 2; 3; 4; 5];;

let rec print_list = function
    [] -> ()
    | h::t ->
        print_int h; 
        print_string " ";
        print_list t

let () =
  print_list hs
  
