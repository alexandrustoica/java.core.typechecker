open CoreTypes
open Printer

let () = print_endline (Printer.print (
	Program([
		ClassDeclaration(
			"Main", "Object", [
				FieldDeclaration((CoreBool false), "a");
				FieldDeclaration((CoreString "test"), "b");
			],
			MethodDeclarations([""; ""; ""]))
])));;