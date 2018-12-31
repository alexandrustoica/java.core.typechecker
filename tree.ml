
open Core.Std;;

type methodDeclaration = MethodDeclaration of string;;
type fieldDeclaration = FieldDeclaration of string;;
type classDeclaration = ClassDeclaration of 
    (string * string * fieldDeclaration list * methodDeclaration list);; 
type program = Program of classDeclaration list;;


let print_class_declaration classDeclaration =
  match classDeclaration with
  | ClassDeclaration (className, superClassName, fields, methods) ->
    "class " ^ className ^ " extends " ^ superClassName;;  


let print_program program = 
  match program with
  | Program (classDeclarations) -> List.fold ~init:"" ~f:(^) (List.map print_class_declaration classDeclarations);;


let () = print_endline (print_program (Program [
    ClassDeclaration ( "Test", "Object", [FieldDeclaration "f"], [MethodDeclaration "m"])]));; 
