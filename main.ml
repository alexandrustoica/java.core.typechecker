open CoreTypes
open Printer
open Ast


let rec print_ast tree = match tree with
	| SystemType value -> match value with 
		| CoreType value -> Printer.type_to_string value
		| UserDefinedType value -> value
		| _ -> "Nothing"
	| _ -> "test";;


let () = print_endline 
	(print_ast (SystemType(CoreType(CoreString))));;

(*
let () = print_endline (Printer.print (
	Program([
		ClassDeclaration(
			"Main", "Object", [
				{systemType = CoreType(CoreUnit); name = "a"};
				{systemType = UserDefinedType("Test"); name = "b"}
			],[
				{
					systemType = CoreType(CoreUnit); name = "functionTest";
					parameters = [Parameter(CoreType(CoreInt), "param")]; expression = "test"
				}
			])
])));;
*)